{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use forM_" #-}
module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C

import Data.IORef ( IORef, newIORef, readIORef, writeIORef , modifyIORef)
import Data.List.Split (chunksOf)

import Minesweeper

-- data State = Menu | Playing | GameOver deriving Eq

main :: IO ()
main = do
    startGUI defaultConfig setup


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

    gridRef <- liftIO $ newIORef $ initGrid 5 10 0
    uiGrid <- makeGrid window gridRef 5

    getBody window #+
        [
            element title,
            element uiGrid
        ]

    return ()


makeGrid :: Window -> IORef Grid -> Int -> UI Element
makeGrid win gridRef n = do
    (Grid _ cells) <- liftIO $ readIORef gridRef
    C.grid $ map (makeRow win) (chunksOf n cells)
        where
            makeRow win chunk = map (makeCell win) chunk
            makeCell win c = uiCell win c gridRef

-- todo use IORef to update board
uiCell :: Window -> Cell -> IORef Grid -> UI Element
uiCell win (i, _, c) gridRef = do
    square <- UI.canvas 
        # set UI.id_ (show i)
        # set UI.style [
            ("width", "30px"),
            ("height", "30px"),
            ("background-color", "lightgrey"),
            ("border", "1px solid black"),
            ("text-align", "center"),
            ("font-size", "20px"),
            ("font-family", "sans-serif"),
            ("color", "black")
        ] 
    on UI.click square $ \_ -> do
        -- liftIO $ modifyIORef gridRef (blockReveal i)
        grid <- liftIO $ readIORef gridRef
        revCells i grid win
        return ()
    return square


revCells :: Int -> Grid -> Window -> UI ()
revCells index (Grid n cells) win = do
    let indexes = blockReveal' (Grid n cells) index
    mapM_ (updateCell cells win) indexes

updateCell :: [Cell] -> Window -> Int -> UI ()
updateCell cells win i = do
    let (_, _, c) = cells !! i
    color <- case c of
        Bomb -> return "red"
        Empty _ -> return "white"
    el <- UI.getElementById win (show i)
    case el of 
        Nothing -> return ()
        Just cell -> do 
            element cell 
                # set UI.style [("background-color", color)]
                # set UI.text (show c)
            return ()
    return ()


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