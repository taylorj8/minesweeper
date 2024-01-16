module Main where

import Minesweeper
import Components
import Solver
import Util

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C

import qualified Data.Vector as V
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Control.Monad (replicateM, when)


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

    -- set up the grid
    -- contains size of grid, cells and top bar elements
    -- restart button left out as it is never changed
    title <- makeTitle
    bombCounter <- topCell (show numBombs)
    squares <- replicateM (size*size) uiCell
    gridRef <- liftIO $ newIORef $ emptyGrid size (title, bombCounter) squares

    -- initialise state and set onclick functions for the cells
    stateRef <- liftIO $ newIORef $ GameStart numBombs
    seedRef <- liftIO $ newIORef 0
    setOnClick gridRef stateRef seedRef

    testRef <- liftIO $ newIORef 0
    -- set up restart button and arrange in a row
    restartButton <- topCell "↺"
    on UI.click restartButton $ \_ -> handleRestart gridRef stateRef numBombs testRef seedRef
    let topRow = UI.row [element bombCounter, element title, element restartButton] # set UI.style [("margin", "auto")]

    -- set up solve buttons
    solveButton <- makeSolveButton "Play Move"
    autoButton <- makeSolveButton "Auto Play"
    solveRef <- liftIO $ newIORef 0
    on UI.click solveButton $ \_ -> solve gridRef stateRef solveRef testRef seedRef
    on UI.click autoButton $ \_ -> autoSolve gridRef stateRef solveRef testRef seedRef autoButton

    let bottomRow = UI.row [element solveButton, element autoButton] # set UI.style [("margin", "auto")]

    getBody window #+
        [
            UI.div #+ [topRow],
            displayGrid squares size,
            UI.div #+ [bottomRow]
        ]
        
    return ()


-- set up click handlers for each cell
-- left click to reveal cell, right click to place flag
setOnClick :: IORef Grid -> IORef GameState -> IORef Int -> UI ()
setOnClick gridRef stateRef seedRef = do
    (Grid _ _ cells) <- liftIO $ readIORef gridRef
    mapM_ (clickHandlers gridRef) cells
        where
            clickHandlers gridRef (Cell i square state _) = do
                on UI.click square $ \_ -> clickCell i gridRef stateRef seedRef
                on UI.contextmenu square $ \_ -> flagCell i gridRef stateRef
