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
    let grid = initGrid size 10 0 squares
    setOnClick grid
    gridRef <- liftIO $ newIORef grid

    getBody window #+
        [
            element title,
            makeGrid squares size
        ]

    return ()


makeGrid :: [Element] -> Int -> UI Element
makeGrid squares n = UI.grid $ chunksOf n (map element squares)
        

setOnClick :: IORef Grid-> UI ()
setOnClick gridRef = do 
    (Grid n cells) <- liftIO $ readIORef gridRef
    mapM_ (setOnClick' (Grid n cells)) cells


setOnClick' :: Grid -> Cell -> UI ()
setOnClick' grid (Cell i square _ _) = do
    on UI.click square $ \_ -> revealCells i grid
    on UI.contextmenu square $ \_ -> flagCell i grid

    -- liftIO $ modifyIORef gridRef (blockReveal i)
    -- updateCells [i] $ zip squares [0..]


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
revealCells :: Int -> Grid -> UI ()
revealCells index (Grid n cells) = mapM_ update cells
    where
        indexes = blockReveal (Grid n cells) index
        update (Cell i square state typ) = if i `elem` indexes 
            then case state of
                Hidden -> case typ of
                    Bomb -> do -- todo end game
                        element square 
                            # set UI.style [("background-color", "red")]
                            # set UI.text "💣"
                    (Empty numBombs) -> element square 
                            # set UI.style [("background-color", "white"), ("color", textColor numBombs)]
                            # set UI.text (show typ)
                _ -> return square
            else return square


flagCell :: Int -> IORef Grid -> UI Element
flagCell index gridRef = do
    (Grid n cells) <- liftIO $ readIORef gridRef
    let (Cell _ square state _) = cells !! index

    case state of
        Hidden -> element square # set UI.text "🚩"
        Flagged -> element square # set UI.text ""
        _ -> return square
    

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

-- updateCell :: [Cell] -> Window -> Int -> UI ()
-- updateCell cells win i = do
--     let (_, _, c) = cells !! i
--     color <- case c of
--         Bomb -> return "red"
--         Empty _ -> return "white"
--     el <- UI.getElementById win (show i)
--     case el of 
--         Nothing -> return ()
--         Just cell -> do 
--             element cell 
--                 # set UI.style [("background-color", color)]
--                 # set UI.text (show c)
--             return ()
--     return ()



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