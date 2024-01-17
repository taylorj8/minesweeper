{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
module Solver where

import Util
import Minesweeper

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (on)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import qualified Data.Vector as V
import Control.Monad (forever, unless)
import Control.Concurrent ( threadDelay )
import Data.List (subsequences, partition, nub, groupBy, sortBy, minimumBy)
import Math.Combinatorics.Exact.Binomial (choose)
import Data.Function (on)
import Data.Ord (comparing)


-- return true if a move was performed
solve :: IORef Grid -> IORef GameState -> IORef Int -> IORef ProbabilityList -> (Element, Element) -> UI Bool
solve gridRef stateRef currentRef probRef (button, probText) = do
    gameState <- liftIO $ readIORef stateRef
    grid <- liftIO $ readIORef gridRef
    case gameState of
        -- first move - click cell in middle
        GameStart _ -> do
            clickCell (middleIndex grid) gridRef stateRef probText
            return True
        Playing _ -> do
            probs <- liftIO $ readIORef probRef
            case probs of
                None -> do
                    success <- basicSolve gridRef stateRef currentRef probText
                    if success then return True
                    else do
                        element button # set UI.style [("background-color", "LightGoldenRodYellow")]
                        let (probList, tooLarge) = getProbablityList grid gameState
                        liftIO $ print probList
                        liftIO $ writeIORef probRef probList
                        element button # set UI.style [("background-color", "lightgrey")]
                        case (probList, tooLarge) of
                            (_, True) -> do
                                element probText # set UI.text "uncertain"
                                return False
                            (Uncertain (_, prob), False) -> do
                                element probText # set UI.text (show (round (prob*100)) ++ "%")
                                return False
                            _ -> probSolve gridRef stateRef probRef probText
                _ -> probSolve gridRef stateRef probRef probText
        _ -> return False


-- repeatedly call solve until no remaining moves
autoSolve :: IORef Grid -> IORef GameState -> IORef Int -> IORef ProbabilityList -> (Element, Element, Element) -> UI ()
autoSolve gridRef stateRef currentRef probRef (solveButton, probText, autoButton) = do
    element autoButton # set UI.style [("background-color", "LightGoldenRodYellow")]
    autoSolve' gridRef stateRef currentRef probRef
    where
        autoSolve' gridRef stateRef currentRef probRef = do
            continue <- solve gridRef stateRef currentRef probRef (solveButton, probText)
            if continue then
                autoSolve' gridRef stateRef currentRef probRef
            else do
                element autoButton # set UI.style [("background-color", "lightgrey")]
                return ()


-- return the middle index of the grid
-- top left middle in case of even grid
middleIndex :: Grid -> Int
middleIndex (Grid n _ _) = case n `mod` 2 of
    1 -> n * n `div` 2
    _ -> n * n `div` 2 - n `div` 2


-- returns true if move performed
basicSolve :: IORef Grid -> IORef GameState -> IORef Int -> Element -> UI Bool
basicSolve gridRef stateRef currentRef probText = do
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
                    else if n - numFlagged == 0 then makeMove hiddenCells current probText clickCell
                    else if n - numFlagged == length hiddenCells then makeMove hiddenCells current probText flagCell
                    else solveFlags' grid (current+1) (iterations+1)

                _ -> solveFlags' grid (current+1) (iterations+1)
        -- make the supplied move and save the current cell index for next time solve button clicked
        makeMove hiddenCells current probText move = do
            move (index $ head hiddenCells) gridRef stateRef probText
            liftIO $ writeIORef currentRef current
            return True


probSolve :: IORef Grid -> IORef GameState -> IORef ProbabilityList -> Element -> UI Bool
probSolve gridRef stateRef probRef probText = do
    probList <- liftIO $ readIORef probRef
    case probList of
        Certain (safe : rest) [] -> do
            clickCell safe gridRef stateRef probText
            if null rest then liftIO $ writeIORef probRef None
            else liftIO $ writeIORef probRef $ Certain rest []
            return True
        Certain safe (bomb : rest) -> do
            flagCell bomb gridRef stateRef probText
            if null safe && null rest then liftIO $ writeIORef probRef None
            else liftIO $ writeIORef probRef $ Certain safe rest
            return True
        Uncertain (safest, prob) -> do
            liftIO $ print (Uncertain (safest, prob))
            clickCell safest gridRef stateRef probText
            liftIO $ writeIORef probRef None
            return False
        _ -> return False


getProbablityList :: Grid -> GameState -> (ProbabilityList, Bool)
getProbablityList grid state = do
    case state of
        Playing (_, bombsRemaining) -> do
            let (frontierCells, frontierNeighbours, numOthers) = getFrontier grid
            let (arrangements, tooLarge) = generateArrangements frontierCells (length frontierCells) bombsRemaining
            let validArrangements = checkArrangements grid frontierNeighbours arrangements
            let safeCells = getSafeCells validArrangements frontierCells
            (toProbList safeCells $ calculateProbabilities validArrangements bombsRemaining numOthers, tooLarge)
        _ -> (None, False)

toProbList :: [Int] -> [(Int, Float)] -> ProbabilityList
toProbList safe probs = do
    let (bombs, uncertain) = partition ((==1.0) . snd) probs
    if null safe && null bombs then Uncertain $ getSafest uncertain
    else Certain safe (map fst bombs)
    where
        getSafest list = minimumBy (comparing snd) list

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

-- generateArrangements :: [Int] -> Int -> [[Int]]
-- generateArrangements indexes n = filter (\l -> length l <= n) $ subsequences indexes

generateArrangements :: [Int] -> Int -> Int -> ([[Int]], Bool)
generateArrangements indexes numCells numBombs = (take n $ subsequences indexes, tooLarge)
    where 
        m = sum $ map (choose numCells) [0..(min numBombs numCells)]
        n = min m 10000000
        tooLarge = m < 0 || m > 10000000

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
