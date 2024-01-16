{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
module Solver where

import Util
import Minesweeper

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import qualified Data.Vector as V
import Control.Monad (forever)
import Control.Concurrent ( threadDelay )


-- return true if a move was performed
solve :: IORef Grid -> IORef GameState -> IORef Int -> IORef Int -> UI Bool
solve gridRef stateRef currentRef testRef = do
    gameState <- liftIO $ readIORef stateRef
    grid <- liftIO $ readIORef gridRef
    case gameState of
        -- first move - click cell in middle
        GameStart _ -> do 
            clickCell (middleIndex grid) gridRef stateRef
            return True
        Playing _ -> do 
            b <- basicSolve gridRef stateRef currentRef testRef
            total <- liftIO $ readIORef testRef
            liftIO $ print total
            return b
        _ -> return False


-- repeatedly call solve until no remaining moves
autoSolve :: IORef Grid -> IORef GameState -> IORef Int -> IORef Int -> Element -> UI ()
autoSolve gridRef stateRef currentRef testRef button = do
    element button # set UI.style [("background-color", "LightGoldenRodYellow")]
    autoSolve' gridRef stateRef currentRef
    where 
        autoSolve' gridRef stateRef currentRef = do
            result <- solve gridRef stateRef currentRef testRef
            if result then do 
                liftIO $ threadDelay 100000
                autoSolve' gridRef stateRef currentRef
            else do 
                element button # set UI.style [("background-color", "lightgrey")]
                return ()


-- return the middle index of the grid
-- top left middle in case of even grid
middleIndex :: Grid -> Int
middleIndex (Grid n _ _) = case n `mod` 2 of
    1 -> (n * n) `div` 2
    _ -> ((n * n) `div` 2) - (n `div` 2)


-- returns true if move performed
basicSolve :: IORef Grid -> IORef GameState -> IORef Int -> IORef Int -> UI Bool
basicSolve gridRef stateRef currentRef testRef = do
    current <- liftIO $ readIORef currentRef
    grid <- liftIO $ readIORef gridRef
    solveFlags' grid current 0
    where 
        solveFlags' grid cur iterations = do
            -- mod keeps index inside grid
            let current = cur `mod` squareSize grid
            let cell = grid `getCell` current
            -- if no move found after checking every cell, return
            if iterations > squareSize grid then do
                liftIO $ modifyIORef testRef (+ iterations)
                return False
            else case cell of 
                -- if 0 cell or unrevealed, try next cell
                (Cell _ _ Revealed (Empty 0)) -> solveFlags' grid (current+1) (iterations+1)
                (Cell i _ Revealed (Empty n)) -> do
                    -- get hidden cells and number of flagged cells
                    let neighbours = getNeighbours grid i
                    let hiddenCells = filter (\cell -> cellState cell == Hidden) neighbours
                    let numFlagged = length $ filter (\cell -> cellState cell == Flagged) neighbours

                    -- two basic rules (explained in report)
                    -- if neither apply, try next cell
                    if null hiddenCells then solveFlags' grid (current+1) (iterations+1)
                    else if n - numFlagged == 0 then makeMove hiddenCells current clickCell iterations
                    else if n - numFlagged == length hiddenCells then makeMove hiddenCells current flagCell iterations
                    else solveFlags' grid (current+1) (iterations+1)

                _ -> solveFlags' grid (current+1) (iterations+1)
        -- make the supplied move and save the current cell index for next time solve button clicked
        makeMove hiddenCells current move iterations = do 
            move (index $ head hiddenCells) gridRef stateRef
            liftIO $ writeIORef currentRef current
            liftIO $ modifyIORef testRef (+ iterations)
            return True


