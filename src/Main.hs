{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use forM_" #-}
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
import Control.Concurrent
import Data.Maybe (maybe)


main :: IO ()
main = do
    startGUI defaultConfig { jsStatic = Just "static"} setup


setup :: Window -> UI ()
setup window = do
    return window # set title "Minesweeper"
    -- stops context menu appearing when attempting to place flags
    runFunction $ ffi "window.oncontextmenu = function() { return false; }"

    -- initial grid parameters
    let size = 16
    let numBombs = 40

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


    -- set up solve buttons
    solveButton <- makeSolveButton "Play Move"
    autoButton <- makeSolveButton "Auto Play"
    on UI.click solveButton $ \_ -> solve gridRef stateRef solveRef probRef probText
    on UI.click autoButton $ \_ -> autoSolve gridRef stateRef solveRef probRef (probText, autoButton)
    -- bottom row contains solve buttons and probability text
    bottomRow <- UI.row [element solveButton, element probText, element autoButton] # set UI.style [("margin", "auto")]

    uiGrid <- displayGrid squares size
    gridContainer <- UI.div #+ [element uiGrid]
    difficultyRef <- liftIO $ newIORef $ Medium (16, 40)
    difficultyButton <- makeSolveButton "Medium"

    -- set up restart button and arrange in a row
    restartButton <- topCell "↺"
    on UI.click restartButton $ \_ -> handleRestart gridRef stateRef solveRef probRef difficultyRef probText

    on UI.click difficultyButton $ \_ -> changeDifficulty stateRef difficultyRef difficultyButton window gridContainer gridRef probRef probText bombCounter
    topRow <- UI.row [element bombCounter, element difficultyButton, element restartButton] # set UI.style [("margin", "auto")]

    let body = getBody window
    body # setBackgroundStyle "cave.png"
    body #+
        [
            UI.div #+ [element title],
            UI.div #+ [element topRow],
            element gridContainer,
            UI.div #+ [element bottomRow]
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


changeDifficulty :: IORef GameState -> IORef Difficulty -> Element -> Window -> Element -> IORef Grid -> IORef ProbableMove -> Element -> Element -> UI ()
changeDifficulty stateRef difficultyRef difficultyButton window gridContainer gridRef probRef probText bombCounter = do
    gameState <- liftIO $ readIORef stateRef
    case gameState of
        GameStart _ -> do
            difficulty <- liftIO $ readIORef difficultyRef
            let newDifficulty = change difficulty
            element difficultyButton # set UI.text (show newDifficulty)
            liftIO $ print newDifficulty
            liftIO $ writeIORef difficultyRef newDifficulty

            let (size, numBombs) = getParams newDifficulty
            element bombCounter # set UI.text (show numBombs)
            liftIO $ modifyIORef stateRef (updateState numBombs)

            liftIO $ print size

            deleteGrid
            squares <- replicateM (size*size) uiCell
            newUiGrid <- displayGrid squares size

            liftIO $ modifyIORef gridRef (updateGrid size squares)
            setOnClick gridRef stateRef probRef probText
            element gridContainer #+ [element newUiGrid]
            return ()

            where
                change difficulty = case difficulty of
                    Easy _ -> Medium (16, 40)
                    Medium _ -> Hard (22, 99)
                    Hard _ -> Easy (9, 10)
                deleteGrid = do
                    uiGrid <- getElementById window "0"
                    case uiGrid of
                        Just g -> delete g
                        Nothing -> return ()
        _ -> return ()


updateGrid :: Int -> [Element] -> Grid -> Grid
updateGrid size squares grid = do
    let newCells = V.fromList $ zipWith (curry updateSquare) squares [0..]
    grid { size = size, cells = newCells }
    where
        updateSquare (square, index) = Cell index square Hidden (Empty 0)


updateState :: Int -> GameState -> GameState
updateState numBombs gameState = do
    case gameState of
        GameStart _ -> GameStart numBombs
        _ -> gameState