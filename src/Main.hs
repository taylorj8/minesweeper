module Main where

import Minesweeper
import Components
import Solver
import Util

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C

import qualified Data.Vector as V
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Monad (replicateM, when)


main :: IO ()
main = do
    startGUI defaultConfig { jsStatic = Just "static"} setup


setup :: Window -> UI ()
setup window = do
    return window # set title "Minesweeper"
    -- stops context menu appearing when attempting to place flags
    runFunction $ ffi "window.oncontextmenu = function() { return false; }"

    -- grid parameters
    let size = 20
    let numBombs = 80

    -- set up the grid
    -- contains size of grid, cells and top bar elements
    -- restart button left out as it is never changed
    title <- makeTitle
    bombCounter <- topCell (show numBombs)
    squares <- replicateM (size*size) uiCell
    gridRef <- liftIO $ newIORef $ emptyGrid size (title, bombCounter) squares

    -- set up IORefs for solver
    probRef <- liftIO $ newIORef None
    solveRef <- liftIO $ newIORef 0

    -- initialise state and set onclick functions for the cells
    probText <- makeProbText
    stateRef <- liftIO $ newIORef $ GameStart numBombs
    setOnClick gridRef stateRef probRef probText

    -- set up restart button and arrange in a row
    restartButton <- topCell "↺"
    on UI.click restartButton $ \_ -> handleRestart gridRef stateRef solveRef probRef probText numBombs
    let topRow = UI.row [element bombCounter, element title, element restartButton] # set UI.style [("margin", "auto")]

    -- set up solve buttons
    solveButton <- makeSolveButton "Play Move"
    autoButton <- makeSolveButton "Auto Play"
    on UI.click solveButton $ \_ -> solve gridRef stateRef solveRef probRef probText
    on UI.click autoButton $ \_ -> autoSolve gridRef stateRef solveRef probRef (probText, autoButton)
    -- bottom row contains solve buttons and probability text
    let bottomRow = UI.row [element solveButton, element probText, element autoButton] # set UI.style [("margin", "auto")]
    
    getBody window # setBackgroundStyle "cave.png"
    getBody window #+
        [
            UI.div #+ [topRow],
            displayGrid squares size,
            UI.div #+ [bottomRow]
        ]
        
    return ()


-- set up click handlers for each cell
-- left click to reveal cell, right click to place flag
setOnClick :: IORef Grid -> IORef GameState -> IORef ProbableMove -> Element -> UI ()
setOnClick gridRef stateRef probRef probText = do
    (Grid _ _ cells) <- liftIO $ readIORef gridRef
    mapM_ (clickHandlers gridRef) cells
        where
            clickHandlers gridRef (Cell i square state _) = do
                -- if player updates the grid, probable move needs recalculated
                on UI.click square $ \_ -> do 
                    liftIO $ writeIORef probRef None
                    clickCell i gridRef stateRef probText
                on UI.contextmenu square $ \_ -> do 
                    liftIO $ writeIORef probRef None
                    flagCell i gridRef stateRef probText
