{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
module Solver where

import Util
import Minesweeper

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (on)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import qualified Data.Vector as V
import Control.Monad (forever)
import Control.Concurrent ( threadDelay )
import Data.List (subsequences, partition, nub, groupBy, sortBy)
import Math.Combinatorics.Exact.Binomial (choose)
import Data.Function (on)

-- return true if a move was performed
solve :: IORef Grid -> IORef GameState -> IORef Int -> UI Bool
solve gridRef stateRef currentRef = do
    gameState <- liftIO $ readIORef stateRef
    grid <- liftIO $ readIORef gridRef
    case gameState of
        -- first move - click cell in middle
        GameStart _ -> do
            clickCell (middleIndex grid) gridRef stateRef
            return True
        Playing _ -> basicSolve gridRef stateRef currentRef
        _ -> return False


-- repeatedly call solve until no remaining moves
autoSolve :: IORef Grid -> IORef GameState -> IORef Int -> Element -> UI ()
autoSolve gridRef stateRef currentRef button = do
    element button # set UI.style [("background-color", "LightGoldenRodYellow")]
    autoSolve' gridRef stateRef currentRef
    where
        autoSolve' gridRef stateRef currentRef = do
            continue <- solve gridRef stateRef currentRef
            if continue then
                autoSolve' gridRef stateRef currentRef
            else do
                element button # set UI.style [("background-color", "lightgrey")]
                grid <- liftIO $ readIORef gridRef
                state <- liftIO $ readIORef stateRef
                let prob = probSolve grid state
                liftIO $ print prob
                return ()


-- return the middle index of the grid
-- top left middle in case of even grid
middleIndex :: Grid -> Int
middleIndex (Grid n _ _) = case n `mod` 2 of
    1 -> n * n `div` 2
    _ -> n * n `div` 2 - n `div` 2


-- returns true if move performed
basicSolve :: IORef Grid -> IORef GameState -> IORef Int -> UI Bool
basicSolve gridRef stateRef currentRef = do
    current <- liftIO $ readIORef currentRef
    grid <- liftIO $ readIORef gridRef
    solveFlags' grid current 0
    where
        solveFlags' grid cur iterations = do
            -- mod keeps index inside grid
            let current = cur `mod` squareSize grid
            let cell = grid `getCell` current
            -- if no move found after checking every cell, return
            if iterations > squareSize grid then return False
            else case cell of
                -- if 0 cell or unrevealed, try next cell
                (Cell _ _ Revealed (Empty 0)) -> solveFlags' grid (current+1) (iterations+1)
                (Cell i _ Revealed (Empty n)) -> do
                    -- get hidden cells and number of flagged cells
                    let neighbours = getNeighbours grid i
                    let hiddenCells = filter (stateIs Hidden) neighbours
                    let numFlagged = length $ filter (stateIs Flagged) neighbours

                    -- two basic rules (explained in report)
                    -- if neither apply, try next cell
                    if null hiddenCells then solveFlags' grid (current+1) (iterations+1)
                    else if n - numFlagged == 0 then makeMove hiddenCells current clickCell
                    else if n - numFlagged == length hiddenCells then makeMove hiddenCells current flagCell
                    else solveFlags' grid (current+1) (iterations+1)

                _ -> solveFlags' grid (current+1) (iterations+1)
        -- make the supplied move and save the current cell index for next time solve button clicked
        makeMove hiddenCells current move = do
            move (index $ head hiddenCells) gridRef stateRef
            liftIO $ writeIORef currentRef current
            return True


probSolve :: Grid -> GameState -> [(Int, Float)]
probSolve grid state = do
    case state of
        Playing (_, bombsRemaining) -> do
            let (frontierCells, frontierNeighbours, numOthers) = getFrontier grid
            let arrangements = generateArrangements frontierCells bombsRemaining
            let validArrangements = checkArrangements grid frontierNeighbours arrangements
            let safeCells = getSafeCells validArrangements frontierCells
            calculateProbabilities validArrangements bombsRemaining numOthers
        _ -> []

-- get indices of frontier cells along with number of other hidden cells
-- i.e. hidden cells with a revealed neigbhour
-- also returns non-zero neighbours of frontier cells
getFrontier :: Grid -> ([Int], [Cell], Int)
getFrontier grid = do
    let hiddenCells = map index $ V.toList $ V.filter (stateIs Hidden) (cells grid)
    let (frontierIndexes, numOthers) = partition hasEmptyNeighbour hiddenCells
    let frontierNeighbours = nub . filter (stateIs Revealed) $ concatMap (getNeighbours grid) frontierIndexes
    (frontierIndexes, frontierNeighbours, length numOthers)
    where
        hasEmptyNeighbour index = do
            let neighbours = getNeighbours grid index
            any (stateIs Revealed) neighbours


getSafeCells :: [[Int]] -> [Int] -> [Int]
getSafeCells arrangements = filter (\c -> all (notElem c) arrangements)


stateIs :: CellState -> Cell -> Bool
stateIs s c = cellState c == s

generateArrangements :: [Int] -> Int -> [[Int]]
generateArrangements indexes n = filter (\l -> length l <= n) $ subsequences indexes

-- return only valid arrangements
checkArrangements :: Grid -> [Cell] -> [[Int]] -> [[Int]]
checkArrangements grid frontierNeighbours = filter isValidArrangement
    where
        isValidArrangement :: [Int] -> Bool
        isValidArrangement arrangement = do
            let grid' = toggleFlagged arrangement grid
            all (isValid grid') frontierNeighbours
        isValid grid' cell = case cell of
            (Cell index _ _ (Empty n)) -> do
                let surCells = getNeighbours grid' index
                let numFlagged = length $ filter (stateIs Flagged) surCells
                n == numFlagged
            _ -> False
        isVal :: [Int] -> Bool
        isVal arrangement = length arrangement == 3


calculateProbabilities :: [[Int]] -> Int -> Int -> [(Int, Float)]
calculateProbabilities arrangements remainingBombs numOthers = do
    let counts = map (ch . length) arrangements
    let totalArrangements = sum counts
    let weightedCounts = weightedCount arrangements counts
    map (\(i, prob) -> (i, fromIntegral prob / fromIntegral totalArrangements)) weightedCounts
    where
        ch bombsPlaced = numOthers `choose` (remainingBombs - bombsPlaced)
        weightedCount :: [[Int]] -> [Int] -> [(Int, Int)]
        weightedCount arrangements counts = do
            let weightedArrangements = concatMap (\(arr, count) -> zip arr (repeat count)) $ zip arrangements counts
            let groupedArrangements = groupBy ((==) `on` fst) $ sortBy (compare `on` fst) weightedArrangements
            map (\group -> (fst (head group), sum (map snd group))) groupedArrangements
