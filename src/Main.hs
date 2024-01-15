{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use forM_" #-}
module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C

import Data.IORef ( IORef, newIORef, readIORef, writeIORef , modifyIORef)
import Data.List (elemIndex)

import Minesweeper
import Components
import Control.Monad (replicateM, liftM, when)
import Data.Maybe (fromMaybe)

import qualified Data.Vector as V

data GameState
    = GameStart Int
    | Playing (Int, Int)
    | GameOver
    | Win
    deriving Eq

main :: IO ()
main = do
    startGUI defaultConfig setup


setup :: Window -> UI ()
setup window = do
    return window # set title "Minesweeper"
    runFunction $ ffi "window.oncontextmenu = function() { return false; }"

    title <- makeTitle

    let size = 6
    let numBombs = 3
    stateRef <- liftIO $ newIORef $ GameStart numBombs
    bombCounter <- makeBombCounter numBombs
    squares <- replicateM (size*size) uiCell
    gridRef <- liftIO $ newIORef $ emptyGrid size (title, bombCounter) squares
    setOnClick gridRef stateRef

    restartButton <- makeRestartButton
    on UI.click restartButton $ \_ -> handleRestart gridRef stateRef numBombs

    getBody window #+
        [
            row [element bombCounter, element title, element restartButton],
            displayGrid squares size
        ]

    return ()


setOnClick :: IORef Grid -> IORef GameState -> UI ()
setOnClick gridRef stateRef = do
    (Grid n _ cells) <- liftIO $ readIORef gridRef
    mapM_ (clickHandlers gridRef) cells
        where
            clickHandlers gridRef (Cell i square state _) = do
                on UI.click square $ \_ -> onClick i gridRef stateRef
                on UI.contextmenu square $ \_ -> flagCell i gridRef stateRef


onClick :: Int -> IORef Grid -> IORef GameState -> UI ()
onClick index gridRef stateRef = do
    gameState <- liftIO $ readIORef stateRef
    case gameState of
        GameStart numBombs -> do
            (Grid n _ _) <- liftIO $ readIORef gridRef
            liftIO $ writeIORef stateRef $ Playing (n*n - numBombs, numBombs)
            seed <- liftIO sysTime
            liftIO $ modifyIORef gridRef $ resetGrid numBombs index seed
            revealCells index gridRef stateRef
        Playing (t, _) -> do
            grid <- liftIO $ readIORef gridRef
            when (cellState (grid `getCell` index) == Hidden) $ revealCells index gridRef stateRef
        _ -> return ()


revealCells :: Int -> IORef Grid -> IORef GameState -> UI ()
revealCells index gridRef stateRef = do
    grid <- liftIO $ readIORef gridRef
    let indexes = revealIndexes grid index
    liftIO $ print indexes
    let (grid', cellsRevealed) = updateCells indexes grid
    trackRemainingCells grid stateRef cellsRevealed
    mapM_ (revealCell grid stateRef) indexes
    liftIO $ writeIORef gridRef grid'
    return ()

revealCell :: Grid -> IORef GameState -> Int -> UI Element
revealCell grid stateRef index = do
    let (Cell _ square state typ) = cells grid V.! index
    case state of
        Hidden -> do
            case typ of
                Bomb -> do
                    V.mapM_ (fillBomb GameOver) (cells grid)
                    liftIO $ writeIORef stateRef GameOver
                    element square # set UI.style [("background-color", "red")]
                    element (fst $ top grid) # set UI.text "Game Over"
                (Empty numBombs) -> element square
                    # set UI.style [("background-color", "white"), ("color", textColor numBombs)]
                    # set UI.text (show typ)
        _ -> return square

        
trackRemainingCells :: Grid -> IORef GameState -> Int -> UI ()
trackRemainingCells grid stateRef num = do
    state <- liftIO $ readIORef stateRef
    let state' = trackRemainingCells' num state
    when (state' == Win) $ do
        V.mapM_ (fillBomb state') (cells grid)
        element (fst $ top grid) # set UI.text "You Win!"
        element (snd $ top grid) # set UI.text "0"
        return ()
    liftIO $ writeIORef stateRef state'
        where
            trackRemainingCells' num (Playing (t, c)) = case t-num of
                0 -> Win
                _ -> Playing (t-num, c)
            trackRemainingCells' _ state = state


flagCell :: Int -> IORef Grid -> IORef GameState -> UI ()
flagCell index gridRef stateRef = do
    gameState <- liftIO $ readIORef stateRef
    case gameState of
        Playing (_, numBombs) -> do
            grid <- liftIO $ readIORef gridRef
            let cell = cells grid V.! index
            liftIO $ writeIORef gridRef $ toggleFlagged index grid
            case cellState cell of
                Hidden -> do
                    liftIO $ modifyIORef stateRef $ handleCounter (-)
                    element (snd $ top grid) # set UI.text (show $ numBombs-1)
                    element (square cell) # set UI.text "🚩"
                Flagged -> do
                    liftIO $ modifyIORef stateRef $ handleCounter (+)
                    element (snd $ top grid) # set UI.text (show $ numBombs+1)
                    element (square cell) # set UI.text ""
                _ -> element (square cell)
            return ()
        _ -> return ()


handleCounter :: (Int -> Int -> Int) -> GameState -> GameState
handleCounter op (Playing (t, c)) = Playing (t, c `op` 1)
handleCounter _ s = s


handleRestart :: IORef Grid -> IORef GameState -> Int -> UI ()
handleRestart gridRef stateRef numBombs = do
    Grid _ top cells <- liftIO $ readIORef gridRef
    resetTopBar top
    liftIO $ writeIORef stateRef (GameStart numBombs)
    V.mapM_ resetSquare cells
    liftIO $ modifyIORef gridRef $ wipeGrid top
        where
            wipeGrid t (Grid n _ cells) = Grid n t (V.map resetCell cells)
            resetCell (Cell index square _ _) = Cell index square Hidden (Empty 0)
            resetSquare (Cell _ square _ _) = element square
                # set UI.style [("background-color", "lightgrey"), ("color", "black")]
                # set UI.text ""
            resetTopBar (title, counter) = do
                element title # set UI.text "Minesweeper"
                element counter # set UI.text (show numBombs)

textColor :: Int -> String
textColor n = case n of
    1 -> "blue"
    2 -> "green"
    3 -> "red"
    4 -> "purple"
    5 -> "maroon"
    6 -> "turquoise"
    7 -> "black"
    8 -> "grey"
    _ -> "white"


fillBomb :: GameState -> Cell -> UI Element
fillBomb state (Cell _ square _ typ) = do
    case (state, typ) of
        (Win, Bomb) -> element square # set UI.text "🚩" 
        (GameOver, Bomb) -> element square
            # set UI.style [("background-color", "white")]
            # set UI.text "💣"
        _ -> return square
