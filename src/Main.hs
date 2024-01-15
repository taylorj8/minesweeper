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
    = GameStart (Int, Int)
    | Playing Int
    | GameOver
    deriving Eq

main :: IO ()
main = do
    startGUI defaultConfig setup


setup :: Window -> UI ()
setup window = do
    return window # set title "Minesweeper"
    runFunction $ ffi "window.oncontextmenu = function() { return false; }"

    title <- UI.div
        # set UI.text "Minesweeper"
        # set UI.style [
            ("width", "100%"),
            ("height", "50px"),
            ("line-height", "50px"),
            ("text-align", "center"),
            ("font-size", "20px"),
            ("font-family", "sans-serif"),
            ("font-weight", "bold"),
            ("user-select", "none")
        ]

    let size = 16
    let numBombs = 40
    stateRef <- liftIO $ newIORef $ GameStart (size, numBombs)
    squares <- replicateM (size*size) uiCell
    gridRef <- liftIO $ newIORef $ emptyGrid size squares
    setOnClick gridRef stateRef

    restartButton <- makeRestartButton
    on UI.click restartButton $ \_ -> handleRestart gridRef stateRef

    getBody window #+
        [
            element title,
            displayGrid squares size,
            element restartButton
        ]

    return ()


setOnClick :: IORef Grid -> IORef GameState -> UI ()
setOnClick gridRef stateRef = do
    (Grid n cells) <- liftIO $ readIORef gridRef
    mapM_ (clickHandlers gridRef) cells
        where
            clickHandlers gridRef (Cell i square state _) = do
                on UI.click square $ \_ -> onClick i gridRef stateRef
                on UI.contextmenu square $ \_ -> flagCell i gridRef stateRef


-- todo update state
onClick :: Int -> IORef Grid -> IORef GameState -> UI ()
onClick index gridRef stateRef = do
    gameState <- liftIO $ readIORef stateRef
    case gameState of
        GameStart (_, numBombs) -> do
            liftIO $ writeIORef stateRef $ Playing numBombs
            seed <- liftIO sysTime
            liftIO $ modifyIORef gridRef $ resetGrid numBombs index seed 
            revealCells index gridRef stateRef
        Playing _ -> do
            grid <- liftIO $ readIORef gridRef
            when (cellState (grid `getCell` index) == Hidden) $ revealCells index gridRef stateRef
        GameOver -> return ()


revealCells :: Int -> IORef Grid -> IORef GameState -> UI ()
revealCells index gridRef stateRef = do
    grid <- liftIO $ readIORef gridRef
    let indexes = revealIndexes grid index
    liftIO $ print indexes
    mapM_ (update grid) indexes
    liftIO $ writeIORef gridRef $ updateCells indexes grid
    return ()
    where
        update grid index = do
            let (Cell _ square state typ) = cells grid V.! index
            case state of
                Hidden -> do
                    case typ of
                        Bomb -> do
                            V.mapM_ revealBomb (cells grid)
                            liftIO $ writeIORef stateRef GameOver
                            element square # set UI.style [("background-color", "red")]
                        (Empty numBombs) -> element square
                            # set UI.style [("background-color", "white"), ("color", textColor numBombs)]
                            # set UI.text (show typ)
                _ -> return square


flagCell :: Int -> IORef Grid -> IORef GameState -> UI ()
flagCell index gridRef stateRef = do
    gameState <- liftIO $ readIORef stateRef
    case gameState of
        Playing _ -> do
            grid <- liftIO $ readIORef gridRef
            let cell = cells grid V.! index
            liftIO $ writeIORef gridRef $ toggleFlagged index grid
            case cellState cell of
                Hidden -> element (square cell) # set UI.text "🚩"
                Flagged -> element (square cell) # set UI.text ""
                _ -> element (square cell)
            return ()
        _ -> return ()


handleRestart :: IORef Grid -> IORef GameState -> UI ()
handleRestart gridRef stateRef = do
    Grid n cells <- liftIO $ readIORef gridRef
    liftIO $ writeIORef stateRef (GameStart (n, 40))
    V.mapM_ resetSquare cells
    liftIO $ modifyIORef gridRef wipeGrid
        where
        wipeGrid (Grid n cells) = Grid n (V.map resetCell cells)
        resetCell (Cell index square _ _) = Cell index square Hidden (Empty 0)
        resetSquare (Cell _ square _ _) = element square
            # set UI.style [("background-color", "lightgrey"), ("color", "black")]
            # set UI.text ""

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


revealBomb :: Cell -> UI Element
revealBomb (Cell _ square _ typ) = do
    case typ of
        Bomb -> element square
            # set UI.style [("background-color", "white")]
            # set UI.text "💣"
        _ -> return square