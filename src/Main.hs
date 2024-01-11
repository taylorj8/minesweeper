module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C

import Data.IORef ( IORef, newIORef, readIORef, writeIORef , modifyIORef)

import Minesweeper

-- data State = Menu | Playing | GameOver deriving Eq

main :: IO ()
main = do
    startGUI defaultConfig setup


setup window = do
    return window # set title "MineSweeper"

    title <- UI.canvas
        # set UI.width 100
        # set UI.height 50
        # set UI.textFont "20px sans-serif"
        # set UI.style [
            ("border", "solid black 2px"),
            ("background", "#eee")
        ]
        # set UI.text "MineSweeper"
--   state <- liftIO $ newIORef Menu

    gridRef <- liftIO $ newIORef $ initGrid 5 10 0
    revRef <- liftIO $ newIORef $ replicate 25 False


    getBody window #+
        [
            element title,
            makeGrid gridRef revRef
        ]

    return ()


makeGrid :: IORef Grid -> IORef [Bool] -> UI Element
makeGrid gridRef revRef = do
    (Grid _ cells) <- liftIO $ readIORef gridRef
    C.row $ map makeCell cells
        where
            makeCell c = cell False c revRef


cell :: Bool -> Cell -> IORef [Bool] -> UI Element
cell False c revRef = do
    i <- case c of
        Bomb i -> return i
        Empty i _ -> return i
        Null -> return 0
    button <- UI.canvas # set UI.style [
            ("width", "30px"),
            ("height", "30px"),
            ("background-color", "grey"),
            ("border", "1px solid black")
        ]
    on UI.click button $ \_ -> do
        revealCell i revRef
        liftIO $ print c
        return ()
    return button
cell True c _ = do
    color <- case c of
        Bomb _ -> return "red"
        Empty _ _ -> return "white"
        Null -> return "white"
    UI.div # set UI.style [
            ("width", "30px"),
            ("height", "30px"),
            ("background-color", color),
            ("border", "1px solid black")
        ]


revealCell :: Int -> IORef [Bool] -> UI ()
revealCell i revRef = do
    rev <- liftIO $ readIORef revRef
    liftIO $ writeIORef revRef (take i rev ++ [True] ++ drop (i+1) rev)
    return ()


-- populateGrid :: Int -> [[UI Element]]
-- populateGrid n = replicate n (replicate n $ cell False Null)

-- initialGrid :: Int -> UI [Element]
-- initialGrid n = [cell False Null | _ <- [1..n*n]]

-- uiGrid :: Grid -> [Bool] -> [UI Element]
-- uiGrid (Grid n cells) revealed = zipWith cell revealed cells