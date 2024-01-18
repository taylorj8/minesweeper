module Main where

import Minesweeper
import Components
import Solver
import Util

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C

import qualified Data.Vector as V
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Control.Monad (replicateM, when, unless)


main :: IO ()
main = do
    startGUI defaultConfig setup


setup :: Window -> UI ()
setup window = do
    return window # set title "Minesweeper"
    -- stops context menu appearing when attempting to place flags
    runFunction $ ffi "window.oncontextmenu = function() { return false; }"

    -- board parameters
    let size = 16
    let numBombs = 40

    parRef <- liftIO $ newIORef True
    seedRef <- liftIO $ newIORef 112

    -- set up the grid
    -- contains size of grid, cells and top bar elements
    -- restart button left out as it is never changed
    title <- makeTitle
    bombCounter <- topCell (show numBombs)
    squares <- replicateM (size*size) uiCell
    gridRef <- liftIO $ newIORef $ emptyGrid size (title, bombCounter) squares

    -- initialise state and set onclick functions for the cells
    probText <- makeProbText
    stateRef <- liftIO $ newIORef $ GameStart numBombs
    setOnClick gridRef stateRef probText seedRef

    -- set up refs for solver
    probRef <- liftIO $ newIORef None
    solveRef <- liftIO $ newIORef 0

    -- set up restart button and arrange in a row
    restartButton <- topCell "↺"
    on UI.click restartButton $ \_ -> do
        liftIO $ modifyIORef parRef not
        usePar <- liftIO $ readIORef parRef
        when usePar $ liftIO $ modifyIORef seedRef (+1)
        seed <- liftIO $ readIORef seedRef
        liftIO $ print $ "seed: " ++ show seed
        handleRestart gridRef stateRef solveRef probRef probText numBombs
    let topRow = UI.row [element bombCounter, element title, element restartButton] # set UI.style [("margin", "auto")]

    -- set up solve buttons
    solveButton <- makeSolveButton "Play Move"
    autoButton <- makeSolveButton "Auto Play"
    on UI.click solveButton $ \_ -> solve gridRef stateRef solveRef probRef probText parRef seedRef
    on UI.click autoButton $ \_ -> autoSolve gridRef stateRef solveRef probRef (probText, autoButton) parRef seedRef

    let bottomRow = UI.row [element solveButton, element probText, element autoButton] # set UI.style [("margin", "auto")]

    getBody window #+
        [
            UI.div #+ [topRow],
            displayGrid squares size,
            UI.div #+ [bottomRow]
        ]
        
    return ()


-- set up click handlers for each cell
-- left click to reveal cell, right click to place flag
setOnClick :: IORef Grid -> IORef GameState -> Element -> IORef Int -> UI ()
setOnClick gridRef stateRef probText sRef = do
    (Grid _ _ cells) <- liftIO $ readIORef gridRef
    mapM_ (clickHandlers gridRef) cells
        where
            clickHandlers gridRef (Cell i square state _) = do
                on UI.click square $ \_ -> clickCell sRef i gridRef stateRef probText
                on UI.contextmenu square $ \_ -> flagCell i gridRef stateRef probText
