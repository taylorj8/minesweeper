{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use forM_" #-}
module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C

import Data.IORef ( IORef, newIORef, readIORef, writeIORef , modifyIORef)
import Data.List.Split (chunksOf)
import Data.List (elemIndex)

import Minesweeper
import Control.Monad (replicateM, liftM)
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


    getBody window #+
        [
            element title,
            displayGrid squares size
        ]

    return ()


displayGrid :: [Element] -> Int -> UI Element
displayGrid squares n = UI.div #+ [
        UI.grid (chunksOf n $ map element squares) 
            # set UI.style [("margin", "auto"), ("border", "1px solid black")]
    ]
        
setOnClick :: IORef Grid -> IORef GameState -> UI ()
setOnClick gridRef stateRef = do 
    (Grid n cells) <- liftIO $ readIORef gridRef
    mapM_ (clickHandlers gridRef) cells
        where 
            clickHandlers gridRef (Cell i square state _) = do
                on UI.click square $ \_ -> onClick i gridRef stateRef
                on UI.contextmenu square $ \_ -> flagCell i gridRef stateRef


uiCell :: UI Element
uiCell = UI.div # set UI.style [
        ("width", "25px"),
        ("height", "25px"),
        ("line-height", "25px"),
        ("background-color", "lightgrey"),
        ("border", "1px solid black"),
        ("text-align", "center"),
        ("font-size", "15px"),
        ("font-family", "sans-serif"),
        ("font-weight", "bold"),
        ("display", "inline-block"),
        ("vertical-align", "top"),
        ("user-select", "none")
    ] 


-- todo update state
onClick :: Int -> IORef Grid -> IORef GameState -> UI ()
onClick index gridRef stateRef = do
    gameState <- liftIO $ readIORef stateRef
    case gameState of
        GameStart (size, numBombs) -> do
            liftIO $ writeIORef stateRef $ Playing numBombs
            liftIO $ modifyIORef gridRef $ resetGrid numBombs index
            revealCells index gridRef stateRef
        Playing _ -> revealCells index gridRef stateRef
        GameOver -> return ()

revealCells :: Int -> IORef Grid -> IORef GameState -> UI ()
revealCells index gridRef stateRef = do
    grid <- liftIO $ readIORef gridRef
    let indexes = revealIndexes grid index
    liftIO $ print indexes
    mapM_ (update grid) indexes
    liftIO $ writeIORef gridRef $ updateCells indexes grid
    where
        update grid index = do
            let (Cell _ square state typ) = cells grid V.! index
            case state of
                Hidden -> do
                    case typ of
                        Bomb -> do 
                            V.mapM_ revealBombs (cells grid) -- todo end game
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


revealBombs :: Cell -> UI Element
revealBombs (Cell _ square _ typ) = do
    case typ of
        Bomb -> element square 
            # set UI.style [("background-color", "white")]
            # set UI.text "💣"
        _ -> return square