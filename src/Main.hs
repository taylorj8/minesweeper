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

    getBody window #+
        [
            element title,
            makeGrid gridRef 5
        ]

    return ()


makeGrid :: IORef Grid -> Int -> UI Element
makeGrid gridRef n = do
    (Grid _ cells) <- liftIO $ readIORef gridRef
    C.grid $ map makeRow (chunksOf n cells)
        where
            makeRow chunk  = map makeCell chunk
            makeCell c = cell c revRef


cell :: Cell -> IORef Grid -> UI Element
cell c gridRef = case c of
        Bomb i True -> do
        Empty i _ _ -> do
    button <- UI.canvas # set UI.style [
            ("width", "30px"),
            ("height", "30px"),
            ("background-color", "grey"),
            ("border", "1px solid black")
        ]
    on UI.click button $ \_ -> do
        liftIO $ modifyIORef gridRef (revealCell i)
        liftIO $ print c
        return ()
    return button


-- cell' :: Cell -> IORef Grid -> UI Element
-- cell' c gridRef = do
--     i <- case c of
--         Bomb i _ -> return i
--         Empty i _ _ -> return i
--     button <- UI.canvas # set UI.style [
--             ("width", "30px"),
--             ("height", "30px"),
--             ("background-color", "grey"),
--             ("border", "1px solid black")
--         ]
--     on UI.click button $ \_ -> do
--         liftIO $ modifyIORef gridRef (revealCell i)
--         liftIO $ print c
--         return ()
--     return button
-- cell c _ = do
--     color <- case c of
--         Bomb _ _ -> return "red"
--         Empty {} -> return "white"
--     UI.div # set UI.style [
--             ("width", "30px"),
--             ("height", "30px"),
--             ("background-color", color),
--             ("border", "1px solid black")
--         ]


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