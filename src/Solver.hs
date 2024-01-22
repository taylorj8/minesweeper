{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use zipWith" #-}
module Solver where

import Util
import Minesweeper

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (on)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Vector as V
import Data.List (partition, nub, groupBy, sortBy, minimumBy, find, union, (\\))
import Data.Function (on)
import Data.Ord (comparing, Down (Down))
import Data.Ratio ((%))
import Control.Concurrent (threadDelay, forkIO, newEmptyMVar, putMVar, takeMVar)
import qualified Data.Set as S
import Data.Vector (uncons)


-- figures out next move and performs it if completely safe
-- return true if a move was performed
solve :: IORef Grid -> IORef GameState -> IORef Int -> IORef ProbableMove -> Element -> UI Bool
solve gridRef stateRef currentRef probRef probText = do
    gameState <- liftIO $ readIORef stateRef
    grid <- liftIO $ readIORef gridRef
    case gameState of
        -- first move - click cell in middle
        GameStart _ -> do
            clickCell (middleIndex grid) gridRef stateRef probText
            return True
        Playing (_, bombsRemaining) -> do
            -- if no bombs left, click remaining cells
            if bombsRemaining == 0 then do
                clickRemaining gridRef stateRef currentRef probText
                return True
            else do
                probs <- liftIO $ readIORef probRef
                case probs of
                    -- if no probabilistic move calculated follow logic rules
                    None -> do
                        move <- logicSolve gridRef stateRef probRef currentRef probText
                        case move of
                            Certain guaranteedCells -> takeCertainMove gridRef stateRef probRef probText guaranteedCells
                            _ -> do
                                -- if logical move can't be found, calculate probabilistic move
                                element probText # set UI.text "Calculating"
                                probList <- liftIO $ findProbableMove grid bombsRemaining
                                liftIO $ writeIORef probRef probList
                                case probList of
                                    -- if move found is uncertain or naive, show probability but don't take move
                                    -- move is taken on next solve
                                    Uncertain (_, prob) -> do
                                        element probText # set UI.text (show (round (prob*100)) ++ "%")
                                        return False
                                    Naive (_, prob) -> do
                                        element probText # set UI.text ("~" ++ (show (round (prob*100)) ++ "%"))
                                        return False
                                    -- otherwise make move
                                    Certain guaranteedCells -> do
                                        element probText # set UI.text ""
                                        takeCertainMove gridRef stateRef probRef probText guaranteedCells
                                    _ -> do
                                        element probText # set UI.text "None"
                                        return False
                    -- uncertain/naive moves from last turn are taken here
                    -- also certain moves if more than one found
                    _ -> takeProbableMove gridRef stateRef probRef currentRef probText
        _ -> return False


-- repeatedly call solve until no remaining moves
autoSolve :: IORef Grid -> IORef GameState -> IORef Int -> IORef ProbableMove -> (Element, Element) -> UI ()
autoSolve gridRef stateRef currentRef probRef (probText, autoButton) = do
    -- highlight button while auto solver running
    element autoButton # set UI.style [("background-color", "Khaki")]
    autoSolve' gridRef stateRef currentRef probRef
    where
        autoSolve' gridRef stateRef currentRef probRef = do
            -- repeat unless false returned, i.e. move not taken
            -- allows player to choose whether to take chance
            continue <- solve gridRef stateRef currentRef probRef probText
            if continue then do
                -- liftIO $ threadDelay 75000  -- delay for dramatic effect
                autoSolve' gridRef stateRef currentRef probRef
            else do
                -- unhighlight button to show stop
                element autoButton # set UI.style [("background-color", "lightgrey")]
                return ()


-- handles case of cells stuck behind row of bombs
clickRemaining :: IORef Grid -> IORef GameState -> IORef Int -> Element -> UI ()
clickRemaining gridRef stateRef currentRef probText = do
    cur <- liftIO $ readIORef currentRef
    clickRemaining' cur
    where
        clickRemaining' index = do
            grid <- liftIO $ readIORef gridRef
            let index' = index `mod` totalSize grid
            case cellState (grid `getCell` index') of
                Hidden -> do
                    liftIO $ writeIORef currentRef (index' + 1)
                    clickCell index' gridRef stateRef probText
                _ -> clickRemaining' (index' + 1)


-- try to find a move based on simple logic rules
-- any moves found are always safe
logicSolve :: IORef Grid -> IORef GameState -> IORef ProbableMove -> IORef Int -> Element -> UI ProbableMove
logicSolve gridRef stateRef probRef currentRef probText = do
    current <- liftIO $ readIORef currentRef
    grid <- liftIO $ readIORef gridRef
    applyRules grid current 0
    where
        applyRules grid cur iterations = do
            -- mod keeps index inside grid
            let current = cur `mod` totalSize grid
            let cell = grid `getCell` current
            -- if no move found after checking every cell, return
            if iterations > totalSize grid then return None
            else case cell of
                -- if 0 cell or unrevealed, try next cell
                (Cell _ _ Revealed (Empty 0)) -> applyRules grid (current+1) (iterations+1)
                (Cell i _ Revealed (Empty num)) -> do
                    -- get hidden cells and number of flagged cells
                    let neighbours = getNeighbours grid i
                    let hiddenCells = map index $ filter (stateIs Hidden) neighbours
                    let numRemaining = num - length (filter (stateIs Flagged) neighbours)

                    -- two basic rules (explained in report)
                    -- if neither apply or cell already has all cells revealed try next cell 
                    if null hiddenCells then applyRules grid (current+1) (iterations+1)
                    else if numRemaining == 0 then returnCertain hiddenCells current True
                    else if numRemaining == length hiddenCells then returnCertain hiddenCells current False
                    else if numRemaining == 1 then do
                        let (frontier, neighbours) = getFrontier grid i
                        case combineProbs $ map (findSafeCells grid frontier) neighbours of
                            Certain x -> do
                                liftIO $ print x
                                liftIO $ writeIORef currentRef current
                                return $ Certain x
                            _ -> applyRules grid (current+1) (iterations+1)
                    else applyRules grid (current+1) (iterations+1)

                _ -> applyRules grid (current+1) (iterations+1)
        -- make the supplied move and save the current cell index for next time solve button clicked
        returnCertain hiddenCells current safe = do
            liftIO $ writeIORef currentRef current
            if safe then return $ Certain ([], hiddenCells) else return $ Certain (hiddenCells, [])
        getFrontier grid i =
            let (frontier, neighbours) = partition (stateIs Hidden) $ filter (not . stateIs Flagged) $ getNeighbours grid i
            in (S.fromList $ map index frontier, neighbours)


findSafeCells :: Grid -> S.Set Int -> Cell -> ProbableMove
findSafeCells grid s1 cell = case cell of
    (Cell i _ _ (Empty num)) -> do
        let neighbours = getNeighbours grid i
        let s2 = S.fromList $ map index $ filter (stateIs Hidden) neighbours
        let numRemaining = num - length (filter (stateIs Flagged) neighbours)
        if s1 `S.isProperSubsetOf` s2
        then let diff = S.toList $ S.difference s2 s1 in
            case (numRemaining, length diff) of
            (1, _) -> Certain ([], diff)
            (n, m) | n == m+1 -> Certain (diff, [])
            _ -> None
        else None
    _ -> None


-- patternSolve :: Grid -> NeighbourCell -> IO ProbableMove
-- patternSolve grid cell = do
--     case cell of
--         (index, 1, frontierCells) -> do
--             let neighbours = neighbourCells `filterByIndexes` findNeighbours index (size grid)
--             return $ combineProbs $ map (findSafeCells frontierCells) neighbours
--         _ -> return None
--     where
--         findSafeCells :: S.Set Int -> NeighbourCell -> ProbableMove
--         findSafeCells s1 (_, num, s2) = if s1 `S.isProperSubsetOf` s2
--             then let diff = S.toList $ S.difference s2 s1 in
--                 case (num, length diff) of
--                 (1, _) -> Certain ([], diff)
--                 (n, m) | n == m+1 -> Certain (diff, [])
--                 _ -> None
--             else None

-- check the probability list and perform the move inside
-- could be a safe or unsafe move
takeProbableMove :: IORef Grid -> IORef GameState -> IORef ProbableMove -> IORef Int -> Element -> UI Bool
takeProbableMove gridRef stateRef probRef currentRef probText = do
    probList <- liftIO $ readIORef probRef
    case probList of
        -- contains list of guaranteed bombs and safe cells
        Certain guaranteedCells -> takeCertainMove gridRef stateRef probRef probText guaranteedCells
        -- both below contain the safest cell found from unsafe options
        -- click it and update IORef
        Uncertain (safest, prob) -> do
            -- safest is -1 if there was no frontier, i.e. no information about remaining cells
            if safest == -1 then clickRemaining gridRef stateRef currentRef probText
            else clickCell safest gridRef stateRef probText
            liftIO $ writeIORef probRef None
            return True
        Naive (safest, prob) -> do
            clickCell safest gridRef stateRef probText
            liftIO $ writeIORef probRef None
            return True
        _ -> return False


-- makes a move from list of certain moves
takeCertainMove :: IORef Grid -> IORef GameState -> IORef ProbableMove -> Element -> ([Int], [Int]) -> UI Bool
takeCertainMove gridRef stateRef probRef probText guaranteedCells = case guaranteedCells of
    -- flag cell if guaranteed bomb in list
    (flag : rest, safes) -> do
        flagCell flag gridRef stateRef probText
        -- if both empty replace with None
        if null rest && null safes then liftIO $ writeIORef probRef None
        else liftIO $ writeIORef probRef $ Certain (rest, safes)
        return True
    -- click cell if guaranteed safe cell in list
    ([], safe : rest) -> do
        clickCell safe gridRef stateRef probText
        -- if both empty replace with None
        if null rest then liftIO $ writeIORef probRef None
        else liftIO $ writeIORef probRef $ Certain ([], rest)
        return True
    _ -> return False


-- find a probable move
findProbableMove :: Grid -> Int -> IO ProbableMove
findProbableMove grid bombsRemaining = do
    -- get the frontier cells, their neighbours and the number of remaining hidden cells
    let (frontierCells, frontierNeighbours, numOthers) = getFrontier grid
    -- if no frontier cells then no information about any cells
    -- in this case probability is just remaining bombs / remaining cells
    -- -1 tells solver to click first cell
    if null frontierCells then return $ Uncertain (-1, toInteger bombsRemaining % toInteger (max numOthers 1))
    else do
        let neighbourCells = convertCells frontierNeighbours grid frontierCells
        -- too many cells, generating all possible arrangments would take too long so make a naive guess
        -- print $ length frontierCells
        let frontiers = separateFrontiers neighbourCells numOthers (length neighbourCells)
        -- print $ map (length . getFst) frontiers
        -- probabilities <- mapM (getProbableMove bombsRemaining) frontiers
        -- return $ combineProbs probabilities
        return None


-- get indices of frontier cells along with number of other hidden cells
-- i.e. hidden cells with a revealed neigbhour
-- also returns non-zero neighbours of frontier cells
getFrontier :: Grid -> ([Int], [Cell], Int)
getFrontier grid = do
    -- get all unvrevealed cells
    let hiddenCells = map index $ V.toList $ V.filter (stateIs Hidden) (cells grid)
    -- partition by whether the cell is next to a revealed cell
    let (frontierIndexes, others) = partition hasRevealedNeighbour hiddenCells
    -- get all revealed cells that neighbour a frontier cell, filtering out duplicates
    let frontierNeighbours = nub . filter (stateIs Revealed) $ concatMap (getNeighbours grid) frontierIndexes
    (frontierIndexes, frontierNeighbours, length others)
    where
        hasRevealedNeighbour index = do
            -- get neighbours of a cell and return true if any are revealed
            let neighbours = getNeighbours grid index
            any (stateIs Revealed) neighbours


-- divides the frontier into sections that don't affect each other
separateFrontiers :: [NeighbourCell] -> Int -> Int -> [Frontier]
separateFrontiers neighbourCells numOthers originalLength = case neighbourCells of
    cell : rest -> do
        -- get starting point for a frontier
        let partialFrontier = S.toList $ getThr cell
        -- recursively find all connect frontier cells
        let (newFrontier, neighbours) = buildFrontier partialFrontier 0
        -- recursively find the rest, any neighbours already used can be ignoreed fro next recursion
        (newFrontier, neighbours, numOthers + originalLength - length newFrontier) : separateFrontiers (rest \\ neighbours) numOthers originalLength
    _ -> []
    where
        buildFrontier frontier size = do
            -- first get all revealed neighbours of the current frontier
            let neighbours = filterNeighbours frontier neighbourCells
            -- then get the hidden neighbours of those
            -- this gives all the old values, plus some of the surrounding ones
            let newFrontier = S.toList $ S.unions $ map getThr neighbours
            -- check if size increases, if so recurse
            let newSize = length newFrontier
            if newSize > size then buildFrontier newFrontier newSize
            -- when the size stops increasing, all neighbouring frontier cells are found
            else (newFrontier, neighbours)
        -- neighbours are any neighbour cells that contain the cell in their neighbours set
        filterNeighbours partialFrontier = filter (\(_, _, neighbours) -> any (`S.member` neighbours) partialFrontier)


-- convert cells to a different format to more efficiently check for valid arrangements
-- NeighbourCell countain number of unflagged surrounding bombs and list of indices of neighbouring frontier cells
convertCells :: [Cell] -> Grid -> [Int] -> [NeighbourCell]
convertCells frontierNeighbours grid frontierCells = map convertCell frontierNeighbours
    where
        convertCell (Cell index _ _ (Empty num)) = do
            -- get the surrounding cells
            let neighbourIndexes = findNeighbours index (size grid)
            let surCells = map (getCell grid) neighbourIndexes
            -- subtract flagged bombs from num
            let newNum = num - length (filter (stateIs Flagged) surCells)
            -- filter neighbours to only include frontier cells
            let neighbours = S.fromList $ filter (`elem` frontierCells) neighbourIndexes
            (index, newNum, neighbours)
        convertCell _ = (-1, 0, S.empty)


-- given a frontier, get the safest 
getProbableMove :: Int -> Frontier -> IO ProbableMove
getProbableMove remainingBombs (frontierCells, neighbourCells, numOthers) =
    if length frontierCells > 32 && remainingBombs > 12 then return $ Naive $ getNaiveGuess neighbourCells
    else do
        -- generate all possible valid arrangements using backtracking
        arrangements <- findValidArrangements frontierCells remainingBombs neighbourCells
        -- calculate probability of each cell containing a bomb
        return $ toProbList frontierCells $ calculateProbabilities arrangements remainingBombs numOthers


-- find all possible valid arrangements of bombs
-- uses backtracking to stop generating possibilities that contain an invalid subarrangement
-- result doesn't check that too few bombs have been placed, so filter out these cases
-- uses concurrency to reduce calculation time
findValidArrangements :: [Int] -> Int -> [NeighbourCell] -> IO [Arrangement]
findValidArrangements availableCells remainingBombs frontierNeighbours = do
    validArrs <- findValidArrs availableCells remainingBombs [] 8
    return $ filter isValidArrangement validArrs
    where
        findValidArrs :: [Int] -> Int -> Arrangement -> Int -> IO [Arrangement]
        findValidArrs [] _ currentArrangement _ = return [currentArrangement]  -- stop when out of cells
        findValidArrs _ 0 currentArrangement _ = return [currentArrangement]   -- stop when out of bombs
        -- for each available cell, it can either be included or excluded
        -- recursively find all other arrangements in each case and combine results
        findValidArrs (current : rest) remainingBombs currentArrangement 0
            | isValidSubArrangement currentArrangement = do
                r1 <- findValidArrs rest (remainingBombs-1) (current : currentArrangement) 0
                r2 <- findValidArrs rest remainingBombs currentArrangement 0
                return (r1 ++ r2)
            | otherwise = return [tail currentArrangement]
        -- for the first 8 recursions, spawn new threads
        -- once deeper than that, overhead of spawning thread greater than benefit
        findValidArrs (current : rest) remainingBombs currentArrangement remainingThreads
            | isValidSubArrangement currentArrangement = do
                -- mvars to store partial results
                resInclude <- newEmptyMVar
                resExclude <- newEmptyMVar
                -- new fork for branch that includes current
                forkIO $ do
                    res <- findValidArrs rest (remainingBombs-1) (current : currentArrangement) (remainingThreads-1)
                    putMVar resInclude res
                -- new fork for branch that excludes current
                forkIO $ do
                    res <- findValidArrs rest remainingBombs currentArrangement (remainingThreads-1)
                    putMVar resExclude res
                -- once both have returned results, concatenate them
                r1 <- takeMVar resInclude
                r2 <- takeMVar resExclude
                return (r1 ++ r2)
            | otherwise = return [tail currentArrangement]
        -- count the number of bombs in an arrangement and compare to n of each cell
        -- a subarrangement is valid if too many bombs aren't placed beside any neighbour cell
        -- a whole arrangment is valid if the exact right number of bombs are placed by all neighbour cells
        isValidSubArrangement arrangement = all (isValid (>=) arrangement) frontierNeighbours
        isValidArrangement arrangement = all (isValid (==) arrangement) frontierNeighbours
        isValid c arrangement (_, n, neighbours) = n `c` length (filter (`S.member` neighbours) arrangement)


-- calculate the probabilities of each cell from the valid arrangements
-- returns index mapped to probability
-- Integer and Rational used to avoid overflow
calculateProbabilities :: [[Int]] -> Int -> Int -> [(Int, Rational)]
calculateProbabilities arrangements remainingBombs numOthers = do
    if length arrangements == 1 then zip (head arrangements) (repeat 1)
    else do
        -- for each arrangement, get the number of ways to arrange the remaining bombs
        let counts = map (ch . length) arrangements
        let totalArrangements = sum counts
        -- count number of time cell contains a bomb in every possible arrangement
        let weightedCounts = weightedCount arrangements counts
        -- divide by the number of possible arrangments to get a probability for each cell
        map (\(i, prob) -> (i, prob % max totalArrangements 1)) weightedCounts
        where
            ch bombsPlaced = numOthers `choose` (remainingBombs - bombsPlaced)
            weightedCount :: [[Int]] -> [Integer] -> [(Int, Integer)]
            weightedCount arrangements counts = do
                -- zip each arrangement with it the number of ways to arrange the remaining bombs
                -- move that number inside each arrangement i.e. zip it with each index in the arrangement
                let weightedArrangements = concatMap (\(arr, count) -> zip arr (repeat count)) $ zip arrangements counts
                -- sort by the index, then group the indexes into lists
                let groupedArrangements = groupBy ((==) `on` fst) $ sortBy (compare `on` fst) weightedArrangements
                -- finally add the counts to get total number of times each cell contains a bomb in every possible arrangement
                map (\group -> (fst (head group), sum (map snd group))) groupedArrangements
            -- nCk - first convert to Integer to avoid overflow
            choose :: Int -> Int -> Integer
            choose n k = choose' (toInteger n) (toInteger k)
                where
                    choose' _ 0 = 1
                    choose' 0 _ = 0
                    choose' n k = choose' (n-1) (k-1) * n `div` k


-- takes in list of indexes to probabilities
-- partition out the guaranteed bombs
-- any cells that don't appear in the list are safe
toProbList :: [Int] -> [(Int, Rational)] -> ProbableMove
toProbList frontierCells probs = do
    let safe = filter (`notElem` map fst probs) frontierCells
    let (bombs, uncertain) = partition ((==1.0) . snd) probs
    case (bombs, safe, uncertain) of
        ([], [], []) -> None
        -- if no guaranteed bomb, choose the index with the lowest probability
        ([], [], uncertain) -> Uncertain $ getSafest uncertain
        -- otherwise return all guaranteed bombs to be flagged
        (bombs, safe, _) -> Certain (map fst bombs, safe)


-- make a guess based on individual cells
-- not very accurate, but better than waiting 5 minutes for a guess
getNaiveGuess :: [NeighbourCell] -> (Int, Float)
getNaiveGuess cells = getSafest $ map checkCell cells
    where
        checkCell (_, num, neighbours) = (minimum neighbours, fromIntegral num / fromIntegral (length neighbours))


-- helper function to get the safest option
getSafest :: Ord a => [(Int, a)] -> (Int, a)
getSafest = minimumBy (comparing snd)
