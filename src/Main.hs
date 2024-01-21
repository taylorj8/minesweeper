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
    squares <- replicateM 484 uiCell -- enough squares for all difficulties
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

    -- set up grid and difficulty button
    -- grid placed in container so it can be easily replaced when difficulty changes
    uiGrid <- makeGrid squares size
    gridContainer <- UI.div #+ [element uiGrid]

    -- set up difficulty ref and button
    difficultyRef <- liftIO $ newIORef $ Medium (size, numBombs)
    difficultyButton <- makeDifficultyButton

    -- set up restart and difficulty buttons
    restartButton <- topCell "↺"
    on UI.click restartButton $ \_ -> handleRestart gridRef stateRef solveRef probRef difficultyRef probText
    on UI.click difficultyButton $ \_ -> changeDifficulty difficultyRef stateRef gridRef probRef window gridContainer probText bombCounter difficultyButton squares
    topRow <- UI.row [element bombCounter, element difficultyButton, element restartButton] # set UI.style [("margin", "auto")]

    -- set background image and window content
    let body = getBody window
    body # setBackground "cave.png"
    body #+
        [
            UI.div #+ [element title],
            UI.div #+ [element topRow] # set UI.style [("margin-bottom", "8px")],
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


changeDifficulty :: IORef Difficulty -> IORef GameState -> IORef Grid -> IORef ProbableMove -> Window -> Element -> Element -> Element -> Element -> [Element] -> UI ()
changeDifficulty difficultyRef stateRef gridRef probRef window gridContainer probText bombCounter difficultyButton squares = do
    gameState <- liftIO $ readIORef stateRef
    case gameState of
        -- only allow changing difficulty at the start of the game
        GameStart _ -> do
            -- update the difficulty
            difficulty <- liftIO $ readIORef difficultyRef
            let newDifficulty = change difficulty
            element difficultyButton
                # set UI.text (show newDifficulty)
                # set UI.style [("background-color", getColor newDifficulty)]
            liftIO $ writeIORef difficultyRef newDifficulty

            -- update the bomb counter and game state
            let (size, numBombs) = getParams newDifficulty
            element bombCounter # set UI.text (show numBombs)
            liftIO $ modifyIORef stateRef (updateState numBombs)

            -- delete the grid and remake it
            uiGrid <- getElementById window "0"
            maybe (return ()) delete uiGrid
            newUiGrid <- makeGrid squares size

            -- update the grid and re-set the click handlers
            liftIO $ modifyIORef gridRef (updateGrid size squares)
            setOnClick gridRef stateRef probRef probText
            -- place the grid on the page
            element gridContainer #+ [element newUiGrid]
            return ()
                    
        _ -> return ()


-- restarts the game
handleRestart :: IORef Grid -> IORef GameState -> IORef Int -> IORef ProbableMove -> IORef Difficulty -> Element -> UI ()
handleRestart gridRef stateRef solveRef probRef difficultyRef probText = do
    Grid _ top cells <- liftIO $ readIORef gridRef
    difficulty <- liftIO $ readIORef difficultyRef
    let (_, numBombs) = getParams difficulty
    resetTopBar top numBombs  -- reset title and flag count
    V.mapM_ resetSquare cells  -- reset UI
    -- reset IORefs
    element probText # set UI.text ""
    liftIO $ writeIORef stateRef (GameStart numBombs)
    liftIO $ modifyIORef gridRef $ \grid -> grid { cells = V.map resetCell cells }
    liftIO $ writeIORef solveRef 0
    liftIO $ writeIORef probRef None
        where
            -- set cell as hidden and empty
            resetCell (Cell index square _ _) = Cell index square Hidden (Empty 0)
            -- remove text and set color back to grey
            resetSquare (Cell _ square _ _) = element square
                # set UI.style [("background-color", "lightgrey")]
                # set UI.text ""
            -- reset top bar text
            resetTopBar (title, counter) numBombs = do
                element title # set UI.text "Minesweeper"
                element counter # set UI.text (show numBombs)
