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

-- data State = Menu | Playing | GameOver deriving Eq

main :: IO ()
main = do
    startGUI defaultConfig setup


setup :: Window -> UI ()
setup window = do
    return window # set title "Minesweeper"
    runFunction $ ffi "window.oncontextmenu = function() { return false; }"

    title <- UI.canvas
        # set UI.width 200
        # set UI.height 20
        # set UI.textFont "20px sans-serif"
        # set UI.style [
            ("border", "solid black 2px"),
            ("background", "#eee")
        ]
        
    title # UI.fillText "Minesweeper" (10, 32)
--   state <- liftIO $ newIORef Menu

    let size = 5
    squares <- replicateM (size*size) uiCell
    gridRef <- liftIO $ newIORef $ initGrid size 10 0 squares
    setOnClick gridRef

    getBody window #+
        [
            element title,
            displayGrid squares size
        ]

    return ()


displayGrid :: [Element] -> Int -> UI Element
displayGrid squares n = UI.grid $ chunksOf n (map element squares)
        
setOnClick :: IORef Grid -> UI ()
setOnClick gridRef = do 
    (Grid n cells) <- liftIO $ readIORef gridRef
    mapM_ (clickHandlers gridRef) cells
        where 
            clickHandlers gridRef (Cell i square state _) = do
                on UI.click square $ \_ -> revealCells i gridRef
                on UI.contextmenu square $ \_ -> flagCell i gridRef


-- todo use IORef to update board
uiCell :: UI Element
uiCell = UI.div # set UI.style [
        ("width", "35px"),
        ("height", "35px"),
        ("line-height", "35px"),
        ("background-color", "lightgrey"),
        ("border", "1px solid black"),
        ("position", "relative"),
        ("text-align", "center"),
        ("font-size", "20px"),
        ("font-family", "sans-serif"),
        ("font-weight", "bold"),
        ("display", "inline-block"),
        ("vertical-align", "top"),
        ("overflow", "hidden"),
        ("user-select", "none")
    ] 


-- todo update state
-- todo rewrite with Vector accesses
revealCells :: Int -> IORef Grid -> UI ()
revealCells index gridRef = do
    grid <- liftIO $ readIORef gridRef
    let indexes = blockReveal grid index
    mapM_ (update grid) indexes
    liftIO $ writeIORef gridRef $ updateCells indexes grid
    where
        update (Grid _ cells) index = do
            let (Cell _ square state typ) = cells V.! index
            case state of
                Hidden -> do
                    case typ of
                        Bomb -> do -- todo end game
                            element square 
                                # set UI.style [("background-color", "red")]
                                # set UI.text "💣"
                        (Empty numBombs) -> element square 
                                # set UI.style [("background-color", "white"), ("color", textColor numBombs)]
                                # set UI.text (show typ)
                _ -> return square


flagCell :: Int -> IORef Grid -> UI Element
flagCell index gridRef = do
    (Grid n cells) <- liftIO $ readIORef gridRef
    let cell = cells V.! index
    liftIO $ writeIORef gridRef $ toggleFlagged index (Grid n cells)
    case cellState cell of
        Hidden -> element (square cell) # set UI.text "🚩"
        Flagged -> element (square cell) # set UI.text ""
        _ -> element (square cell)


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


-- todo if refCells doesn't work out
-- deleteCells :: Window -> Int -> CellType -> UI ()
-- deleteCells win i cell = do
--     el <- UI.getElementById win (show i)
--     case el of 
--         Nothing -> return ()
--         Just test -> delete test
--     case cell of 
--         Bomb -> return () -- lose condition?
--         Empty n -> do
--             case n of 
--                 0 -> do
--                     let neighbours = findNeighbours i 5
--                     map deleteCells win 
--                     return ()
--                 _ -> return ()



-- revealCell :: Int -> IORef [Bool] -> UI ()
-- revealCell i revRef = do
--     rev <- liftIO $ readIORef revRef
--     liftIO $ writeIORef revRef (take i rev ++ [True] ++ drop (i+1) rev)
--     return ()


-- populateGrid :: Int -> [[UI Element]]
-- populateGrid n = replicate n (replicate n $ cell False Null)

-- initialGrid :: Int -> UI [Element]
-- initialGrid n = [cell False Null | _ <- [1..n*n]]

-- uiGrid :: Grid -> [Bool] -> [UI Element]
-- uiGrid (Grid n cells) revealed = zipWith cell revealed cells