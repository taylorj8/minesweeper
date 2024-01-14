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

    squares <- replicateM 25 uiCell
    gridRef <- liftIO $ newIORef $ initGrid 5 10 0
    mapM_ (setOnClick window gridRef squares) $ zip squares [0..]

    -- uiGrid <- makeGrid window gridRef 5


    getBody window #+
        [
            element title,
            makeGrid squares 5
        ]

    return ()


makeGrid :: [Element] -> Int -> UI Element
makeGrid squares n = UI.grid $ chunksOf n (map element squares)
        

setOnClick :: Window -> IORef Grid -> [Element] -> (Element, Int) -> UI ()
setOnClick win gridRef squares (square, i) = do
    on UI.click square $ \_ -> do
        -- liftIO $ modifyIORef gridRef (blockReveal i)
        grid <- liftIO $ readIORef gridRef
        revCells i grid squares
        -- updateCells [i] $ zip squares [0..]
        return ()


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
        ("color", "black"),
        ("display", "inline-block"),
        ("vertical-align", "top"),
        ("overflow", "hidden"),
        ("user-select", "none")
    ] 


revCells :: Int -> Grid -> [Element] -> UI ()
revCells index (Grid n cells) squares = do
    let indexes = blockReveal (Grid n cells) index
    updateCells indexes $ zip squares [0..]

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

updateCells :: [Int] -> [(Element, Int)] -> UI ()
updateCells indexes els = do
    liftIO $ print indexes
    mapM_ update els
    where
        update (square, i) = if i `elem` indexes 
            then do 
                element square 
                    # set UI.style [("background-color", "white")]
                    # set UI.text (show i)
            else return square

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