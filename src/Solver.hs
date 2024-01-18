{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
module Solver where

import Util
import Minesweeper

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (on)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import qualified Data.Vector as V
import Control.Monad (forever, unless, when, filterM)
import Control.Concurrent ( threadDelay )
import Data.List (subsequences, partition, nub, groupBy, sortBy, minimumBy)
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
        Playing (_, bombsRemaining) -> do
            -- if no bombs left, click all remaining cells
            if bombsRemaining == 0 then do
                clickRemaining gridRef stateRef currentRef probText
                return True
            else do
                probs <- liftIO $ readIORef probRef
                case probs of
                    None -> do
                        success <- basicSolve gridRef stateRef currentRef probText
                        if success then return True
                        else do
                            element probText # set UI.text "Calculating"
                            probList <- getProbablityList grid gameState
                            liftIO $ writeIORef probRef probList
                            element probText # set UI.text ""
                            case probList of
                                Uncertain (_, prob) -> do
                                    element probText # set UI.text (show (round (prob*100)) ++ "%")
                                    return False
                                Naive (_, prob) -> do
                                    element probText # set UI.text ("~" ++ (show (round (prob*100)) ++ "%"))
                                    return False
                                _ -> probSolve gridRef stateRef probRef probText
                    _ -> probSolve gridRef stateRef probRef probText
        _ -> return False


-- handles case of bombs stuck behind row of bombs
clickRemaining :: IORef Grid -> IORef GameState -> IORef Int -> Element -> UI ()
clickRemaining gridRef stateRef currentRef probText = do
    cur <- liftIO $ readIORef currentRef
    clickRemaining' cur
    where
        clickRemaining' index = do
            grid <- liftIO $ readIORef gridRef
            let index' = index `mod` squareSize grid
            case cellState (grid `getCell` index') of
                Hidden -> do
                    liftIO $ writeIORef currentRef (index' + 1)
                    clickCell index' gridRef stateRef probText
                _ -> clickRemaining' (index' + 1)


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
            return True
        Naive (safest, prob) -> do
            clickCell safest gridRef stateRef probText
            liftIO $ writeIORef probRef None
            return True
        _ -> return False


getProbablityList :: Grid -> GameState -> UI ProbabilityList
getProbablityList grid state =
    case state of
        Playing (_, bombsRemaining) -> do
            let (frontierCells, frontierNeighbours, numOthers) = getFrontier grid
            let neighbourCells = convertCells frontierNeighbours grid frontierCells
            if sum (map (choose (length frontierCells)) [1..(min bombsRemaining (length frontierCells))]) > 20000000 then do
                return $ Naive $ getNaiveGuess neighbourCells
            else do
                let arrangements = generateArrangements frontierCells (length frontierCells) bombsRemaining
                -- liftIO $ print tooLarge
                liftIO $ print $ length arrangements
                validArrangements <- checkArrangements neighbourCells arrangements
                liftIO $ print validArrangements
                temp <- calculateProbabilities validArrangements bombsRemaining numOthers
                liftIO $ print temp
                liftIO $ print $ toProbList temp
                return $ toProbList temp
        _ -> return None


getNaiveGuess :: [NeighbourCell] -> (Int, Float)
getNaiveGuess cells = getSafest $ map checkCell cells
    where
        checkCell (num, neighbours) = (head neighbours, fromIntegral num / fromIntegral (length neighbours))


toProbList :: [(Int, Rational)] -> ProbabilityList
toProbList probs =
    case partition ((==1.0) . snd) probs of
        ([], []) -> None
        ([], uncertain) -> Uncertain $ getSafest uncertain
        (bombs, _) -> Certain (map fst bombs)

getSafest :: Ord a => [(Int, a)] -> (Int, a)
getSafest = minimumBy (comparing snd)


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


convertCells :: [Cell] -> Grid -> [Int] -> [NeighbourCell]
convertCells frontierNeighbours grid frontierCells = map convertCell frontierNeighbours
    where
        convertCell (Cell index _ _ (Empty num)) = do
            let surCells = getNeighbours grid index
            let newNum = num - length (filter (stateIs Flagged) surCells)
            let neighbours = filter (`elem` frontierCells) $ findNeighbours index (size grid)
            (newNum, neighbours)
        convertCell _ = (0, [])


stateIs :: CellState -> Cell -> Bool
stateIs s c = cellState c == s


-- generate all possible arrangements of bombs
-- first arrangement can be dropped as it's always the empty list
generateArrangements :: [Int] -> Int -> Int -> [[Int]]
generateArrangements indexes numCells numBombs = filter (\x -> length x <= min numBombs numCells) $ drop 1 $ subsequences indexes

-- subseq :: Int -> [Int] -> [[Int]]
-- subseq _ [] = []
-- subseq n (x : xs) = do
--     x : subseq n xs

-- modified version of subsequences
-- stops recursing when length n is reached
subseq :: Int -> [a] -> [[a]]
subseq _ [] = []
subseq n (x:xs) = [x] : foldr f [] (subseq n xs)
    where f ys r = if length ys < n then ys : (x : ys) : r else ys : r


-- take all possible arrangements and filter out invalid arrangements
checkArrangements :: [NeighbourCell] -> [[Int]] -> UI [[Int]]
checkArrangements frontierNeighbours arrangements =
    return $ filter isValidArrangement arrangements
    where
        isValidArrangement arrangement =
            all (isValid arrangement) frontierNeighbours
        isValid arrangement cell = case cell of
            (n, neighbours) -> do
                -- count the surrounding flags, valid if equal to number in cell
                let numFlagged = length $ filter (`elem` arrangement) neighbours
                n == numFlagged


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
        choose' _ 0 = 1
        choose' 0 _ = 0
        choose' n k = choose' (n-1) (k-1) * n `div` k
