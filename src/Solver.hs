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
import Data.List (subsequences, partition, nub, groupBy, sortBy, minimumBy, sortOn)
import Data.Function (on)
import Data.Ord (comparing)
import Data.Ratio ((%))


-- return true if a move was performed
solve :: IORef Grid -> IORef GameState -> IORef Int -> IORef ProbabilityList -> Element -> UI Bool
solve gridRef stateRef currentRef probRef probText = do
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
                        element probText # set UI.text "Calculating"
                        (probList, tooLarge) <- getProbablityList grid gameState
                        liftIO $ writeIORef probRef probList
                        element probText # set UI.text ""
                        case (probList, tooLarge) of
                            (_, True) -> do
                                element probText # set UI.text "Uncertain"
                                return False
                            (Uncertain (_, prob), False) -> do
                                element probText # set UI.text (show (round (prob*100)) ++ "%")
                                return False
                            _ -> probSolve gridRef stateRef probRef probText
                _ -> probSolve gridRef stateRef probRef probText
        _ -> return False


-- repeatedly call solve until no remaining moves
autoSolve :: IORef Grid -> IORef GameState -> IORef Int -> IORef ProbabilityList -> (Element, Element) -> UI ()
autoSolve gridRef stateRef currentRef probRef (probText, autoButton) = do
    element autoButton # set UI.style [("background-color", "LightGoldenRodYellow")]
    autoSolve' gridRef stateRef currentRef probRef
    where
        autoSolve' gridRef stateRef currentRef probRef = do
            continue <- solve gridRef stateRef currentRef probRef probText
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
        Certain (flag : rest) -> do
            flagCell flag gridRef stateRef probText
            if null rest then liftIO $ writeIORef probRef None
            else liftIO $ writeIORef probRef $ Certain rest
            return True
        Uncertain (safest, prob) -> do
            clickCell safest gridRef stateRef probText
            liftIO $ writeIORef probRef None
            return False
        _ -> return False


getProbablityList :: Grid -> GameState -> UI (ProbabilityList, Bool)
getProbablityList grid state = do
    case state of
        Playing (_, bombsRemaining) -> do
            let (frontierCells, frontierNeighbours, numOthers) = getFrontier grid
            -- let subsets = connectedSubsequences frontierCells (size grid)
            let clusters = sortOn length (frontierCells : groupCells (size grid) frontierCells)
            liftIO $ print $ "Clusters: " ++ show (length clusters)
            handleArrangements frontierNeighbours numOthers (length frontierCells) bombsRemaining clusters
        _ -> return (None, False)
    where
        handleArrangements _ _ _ _ [] = return (None, False)
        handleArrangements frontierNeighbours numOthers l bombsRemaining (frontierCells : rest) = do
            liftIO $ print $ "Cells: " ++ show frontierCells
            liftIO $ print $ "Cell Count: " ++ show (length frontierCells)
            let (arrangements, tooLarge) = generateArrangements frontierCells (length frontierCells) bombsRemaining
            liftIO $ print $ "Arrangements: " ++ show (length arrangements)
            validArrangements <- checkArrangements grid frontierNeighbours arrangements
            liftIO $ print $ "Valid Arrangements: " ++ show (length validArrangements)
            temp <- calculateProbabilities validArrangements bombsRemaining (numOthers + (l - length frontierCells))
            let probList = toProbList temp
            liftIO $ print probList
            liftIO $ print $ id ""
            case probList of
                Certain x -> return (Certain x, True)
                Uncertain x -> if null rest then return (Uncertain x, True)
                    else handleArrangements frontierNeighbours numOthers l bombsRemaining rest
                None -> handleArrangements frontierNeighbours numOthers l bombsRemaining rest


-- group neigbouring cells
groupCells :: Int -> [Int] -> [[Int]]
groupCells _ [] = []
groupCells _ [x] = [[x]]
groupCells n (x:y:xs)
 | x `neighbours` y = (x : head clusters) : tail clusters
 | otherwise = [x] : clusters
 where
   clusters = groupCells n (y:xs)
   neighbours x y = diff == 1 || diff == n where diff = abs (x - y)


toProbList :: [(Int, Rational)] -> ProbabilityList
toProbList probs = do
    case partition ((==1.0) . snd) probs of 
        ([], []) -> None
        ([], uncertain) -> Uncertain $ getSafest uncertain
        (bombs, _) -> Certain (map fst bombs)
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


-- sortFrontierCells :: Int -> [Int] -> [Int]
-- sortFrontierCells _ [] = []
-- sortFrontierCells _ [x] = [x]
-- sortFrontierCells n cells = sortedCells
--     where
--         neighbors = map (\x -> (x, filter (areNeighbours x) cells)) cells
--         sortedNeighbors = sortBy (comparing snd) neighbors
--         sortedCells = map fst sortedNeighbors
--         areNeighbours x y = abs (x - y) == 1 || abs (x - y) == n


stateIs :: CellState -> Cell -> Bool
stateIs s c = cellState c == s


-- generate all possible arrangements
-- if too many, stop at 10,000,000 - this may cause inaccurate predictions
-- first arrangement can be dropped as it's always the empty list
generateArrangements :: [Int] -> Int -> Int -> ([[Int]], Bool)
generateArrangements indexes numCells numBombs = (take n $ drop 1 $ subsequences indexes, tooLarge)
    where 
        m = sum $ map (choose numCells) [0..(min numBombs numCells)]
        n = fromInteger $ min m 10000000
        tooLarge = m > 10000000


-- take all possible arrangements and filter out invalid arrangements
checkArrangements :: Grid -> [Cell] -> [[Int]] -> UI [[Int]]
checkArrangements grid frontierNeighbours arr = do
    liftIO $ print frontierNeighbours
    return $ filter isValidArrangement arr
    where
        isValidArrangement arrangement = do
            -- make grid with cells in arrangement flagged and check if valid
            let grid' = toggleFlagged arrangement grid
            all (isValid grid') frontierNeighbours
        isValid grid' cell = case cell of
            (Cell index _ _ (Empty n)) -> do
                -- count the surrounding flags, valid if equal to number in cell
                let surCells = getNeighbours grid' index
                let numFlagged = length $ filter (stateIs Flagged) surCells
                n == numFlagged
            _ -> False
    

connectedSubsequences :: [Int] -> Int -> [[Int]]
connectedSubsequences cells n = sortOn length $ filter allConnected $ subsequences cells
    where 
        allConnected cells = l > 1 && numConnections cells == (l * 2 - 2) where l = length cells
        numConnections cells = sum $ map (\x -> length $ filter (areNeighbours x) cells) cells
        areNeighbours x y = dist == 1 || dist == n where dist = abs (x - y)


calculateProbabilities :: [[Int]] -> Int -> Int -> UI [(Int, Rational)]
calculateProbabilities arrangements remainingBombs numOthers = do
    let counts = map (ch . length) arrangements
    let totalArrangements = sum counts
    let weightedCounts = weightedCount arrangements counts
    return $ map (\(i, prob) -> (i, prob % totalArrangements)) weightedCounts
    where
        ch bombsPlaced = numOthers `choose` (remainingBombs - bombsPlaced)
        weightedCount :: [[Int]] -> [Integer] -> [(Int, Integer)]
        weightedCount arrangements counts = do
            let weightedArrangements = concatMap (\(arr, count) -> zip arr (repeat count)) $ zip arrangements counts
            let groupedArrangements = groupBy ((==) `on` fst) $ sortBy (compare `on` fst) weightedArrangements
            map (\group -> (fst (head group), sum (map snd group))) groupedArrangements


-- nCk - first convert to Integer to avoid overflow
choose :: Int -> Int -> Integer
choose n k = choose' (toInteger n) (toInteger k)
    where
        choose' n 0 = 1
        choose' 0 k = 0
        choose' n k = choose' (n-1) (k-1) * n `div` k
