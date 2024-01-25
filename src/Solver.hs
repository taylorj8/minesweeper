{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use zipWith" #-}
module Solver where

import Util
import Minesweeper

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (on)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Vector as V
import Data.List (partition, nub, groupBy, sortBy, minimumBy, find, union, (\\), maximumBy)
import Data.Function (on)
import Data.Ord (comparing)
import Data.Ratio ((%))
import Control.Concurrent (threadDelay, forkIO, newEmptyMVar, putMVar, takeMVar)
import qualified Data.Set as S


-- figures out next move and performs it if completely safe
-- return true if a move was performed
solve :: IORef Grid -> IORef GameState -> IORef Int -> IORef Move -> Element -> UI Bool
solve gridRef stateRef currentRef moveRef probText = do
    gameState <- liftIO $ readIORef stateRef
    grid <- liftIO $ readIORef gridRef
    case gameState of
        -- first move - click cell in middle
        GameStart _ -> do
            clickCell (middleIndex grid) gridRef stateRef probText
            return True
        Playing (_, bombsRemaining) ->
            -- if no bombs left, click remaining cells
            if bombsRemaining == 0 then do
                clickRemaining gridRef stateRef currentRef probText
                return True
            else do
                probs <- liftIO $ readIORef moveRef
                case probs of
                    -- if no probabilistic move calculated follow logic rules
                    None -> do
                        logicalMove <- findLogicalMove gridRef stateRef currentRef
                        case logicalMove of
                            Certain guaranteedCells -> takeCertainMove gridRef stateRef moveRef probText guaranteedCells
                            _ -> do
                                -- if logical move can't be found, calculate probabilistic move
                                element probText # set UI.text "Calculating"
                                probableMove <- liftIO $ findProbableMove grid bombsRemaining
                                liftIO $ writeIORef moveRef probableMove
                                case probableMove of
                                    -- if move found is uncertain or naive, show probability but don't take move
                                    -- move is taken on next solve
                                    Uncertain (_, prob) -> do
                                        element probText # set UI.text (show (round (prob*100)) ++ "%")
                                        return False
                                    Naive (_, prob) -> do
                                        element probText # set UI.text ("~" ++ show (round (prob*100)) ++ "%")
                                        return False
                                    -- otherwise make move
                                    Certain guaranteedCells -> do
                                        element probText # set UI.text ""
                                        takeCertainMove gridRef stateRef moveRef probText guaranteedCells
                                    _ -> do
                                        element probText # set UI.text "None"
                                        return False
                    -- uncertain/naive moves from last turn are taken here
                    -- also certain moves if more than one found
                    _ -> takeMove gridRef stateRef moveRef currentRef probText
        _ -> return False


-- repeatedly call solve until no remaining moves
autoSolve :: IORef Grid -> IORef GameState -> IORef Int -> IORef Move -> (Element, Element) -> UI ()
autoSolve gridRef stateRef currentRef moveRef (probText, autoButton) = do
    -- highlight button while auto solver running
    element autoButton # set UI.style [("background-color", "Khaki")]
    autoSolve'
    where
        autoSolve' = do
            -- repeat unless false returned, i.e. move not taken
            -- allows player to choose whether to take chance
            continue <- solve gridRef stateRef currentRef moveRef probText
            if continue then do
                liftIO $ threadDelay 5000  -- delay for dramatic effect
                autoSolve'
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


-- try to find a move based on logic rules
-- any moves found are always certain
findLogicalMove :: IORef Grid -> IORef GameState -> IORef Int -> UI Move
findLogicalMove gridRef stateRef currentRef = do
    grid <- liftIO $ readIORef gridRef
    current <- liftIO $ readIORef currentRef
    -- recursively apply rules to each cell
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

                    -- two basic rules acting on single cells explained in report)
                    if null hiddenCells then applyRules grid (current+1) (iterations+1)
                    else if numRemaining == 0 then returnCertain hiddenCells current True
                    else if numRemaining == length hiddenCells then returnCertain hiddenCells current False
                    -- more complex rules acting on pairs of cells (current cell with each of its neighbours)
                    else
                        let (frontier, neighbours) = getCellsToCheck grid i in
                        case combineMoves $ map (matchCellPatterns numRemaining grid frontier) neighbours of
                            Certain x -> do
                                liftIO $ writeIORef currentRef current
                                return $ Certain x
                            -- if none apply, try next cell
                            _ -> applyRules grid (current+1) (iterations+1)
                _ -> applyRules grid (current+1) (iterations+1)
        -- make the supplied move and save the current cell index for next time solve button clicked
        returnCertain hiddenCells current safe = do
            liftIO $ writeIORef currentRef current
            if safe then return $ Certain ([], hiddenCells) else return $ Certain (hiddenCells, [])
        -- gets all revealed cells connected to any hidden cells of a cell
        getCellsToCheck grid i = do
            let neighbours = getNeighbours grid i
            let (frontier, revNeighbours) = partition (stateIs Hidden) $ filter (not . stateIs Flagged) neighbours
            let furtherRevNeighbours = filter (stateIs Revealed) $ concatMap (getNeighbours grid . index) frontier
            (S.fromList $ map index frontier, nub (revNeighbours ++ furtherRevNeighbours))


-- attempts to find patterns in pairs of cells
matchCellPatterns :: Int -> Grid -> S.Set Int -> Cell -> Move
matchCellPatterns c1 grid s1 cell = case cell of
    (Cell i _ _ (Empty num)) -> do
        -- get neighbours of current cell
        let neighbours = getNeighbours grid i
        -- get a set of hidden neighbours
        let s2 = S.fromList $ map index $ filter (stateIs Hidden) neighbours
        let diff = s2 `S.difference` s1
        -- number - flagged neighbours is its effective number for pattern matching
        -- two rules (explained in report)
        case num - length (filter (stateIs Flagged) neighbours) of
            1 -> if s1 `S.isProperSubsetOf` s2 then Certain ([], S.toList diff) else None
            c2 -> if S.size diff == c2-c1 && (not . S.null) diff then Certain (S.toList diff, []) else None
    _ -> None


-- check the probability list and perform the move inside
-- could be a safe or unsafe move
takeMove :: IORef Grid -> IORef GameState -> IORef Move -> IORef Int -> Element -> UI Bool
takeMove gridRef stateRef moveRef currentRef probText = do
    probList <- liftIO $ readIORef moveRef
    case probList of
        -- contains list of guaranteed bombs and safe cells
        Certain guaranteedCells -> takeCertainMove gridRef stateRef moveRef probText guaranteedCells
        -- both below contain the safest cell found from unsafe options
        -- click it and update IORef
        Uncertain (safest, prob) -> do
            -- safest is -1 if there was no frontier, i.e. no information about remaining cells
            if safest == -1 then clickRemaining gridRef stateRef currentRef probText
            else clickCell safest gridRef stateRef probText
            liftIO $ writeIORef moveRef None
            return True
        Naive (safest, prob) -> do
            clickCell safest gridRef stateRef probText
            liftIO $ writeIORef moveRef None
            return True
        _ -> return False


-- makes a move from list of certain moves
takeCertainMove :: IORef Grid -> IORef GameState -> IORef Move -> Element -> ([Int], [Int]) -> UI Bool
takeCertainMove gridRef stateRef moveRef probText guaranteedCells = case guaranteedCells of
    -- flag cell if guaranteed bomb in list
    (flag : rest, safes) -> do
        flagCell flag gridRef stateRef probText
        -- if both empty replace with None
        if null rest && null safes then liftIO $ writeIORef moveRef None
        else liftIO $ writeIORef moveRef $ Certain (rest, safes)
        return True
    -- click cell if guaranteed safe cell in list
    ([], safe : rest) -> do
        clickCell safe gridRef stateRef probText
        -- if both empty replace with None
        if null rest then liftIO $ writeIORef moveRef None
        else liftIO $ writeIORef moveRef $ Certain ([], rest)
        return True
    _ -> return False


-- find a probable move
findProbableMove :: Grid -> Int -> IO Move
findProbableMove grid bombsRemaining = do
    -- get the frontier cells, their neighbours and the number of remaining hidden cells
    let (frontierCells, frontierNeighbours, others) = getFrontier grid
    let numOthers = length others
    -- if no frontier cells then no information about any cells
    -- in this case probability is just remaining bombs / remaining cells
    -- -1 tells solver to click first cell
    if null frontierCells then return $ Uncertain (-1, toInteger bombsRemaining % toInteger (max numOthers 1))
    else do
        let neighbourCells = convertCells frontierNeighbours grid frontierCells
        -- too many cells, generating all possible arrangments would take too long so make a naive guess
        let frontiers = separateFrontiers neighbourCells numOthers (length neighbourCells)
        -- get possible move for each frontier, along with least number of bombs that could be in that frontier
        moves <- mapM (getProbableMove bombsRemaining) frontiers
        -- if uncertain, calculate probability of bombs in non-frontier hidden cells
        case combineMoves $ map fst moves of
            Certain x -> return $ Certain x
            Uncertain (cell, prob) -> calculateOtherProb moves frontierCells others cell prob
            Naive (cell, prob) -> calculateOtherProb moves frontierCells others cell (realToFrac prob)
            x -> return x
    where
        calculateOtherProb moves frontierCells others cell prob = let numOthers = length others in
            if numOthers > 0 then do
                -- leftover bombs are remaining bombs - bombs in each frontier
                -- this is a worst-case estimate - assumes lowest number possible for each frontier
                let leftoverBombs = bombsRemaining - sum (map snd moves)
                let otherProb = toInteger leftoverBombs % toInteger numOthers
                if prob <= otherProb && otherProb /= 1 then return $ Uncertain (cell, prob)
                else case otherProb of
                        -- if probability for other cells 0 or 1, add all others to certain move
                        0 -> return $ Certain ([], others)
                        1 -> return $ Certain (others, [])
                        -- if it is lower, find the best cell - cell that touches the most frontier cells
                        -- this should give the best chance of choosing a cell that gives more information
                        x -> let bestCell = bestHiddenCell grid others frontierCells in
                                 return $ Uncertain (bestCell, x)
            else return $ Uncertain (cell, prob)


-- pick the cell that neigbours the most frontier cells
bestHiddenCell :: Grid -> [Int] -> [Int] -> Int
bestHiddenCell grid others frontierCells = maximumBy (comparing countNeighbours) others
    where
        countNeighbours i = length (filter (`elem` findNeighbours i (size grid)) frontierCells)
            + length (filter (stateIs Flagged) $ getNeighbours grid i)



-- get indices of frontier cells along with number of other hidden cells
-- i.e. hidden cells with a revealed neigbhour
-- also returns non-zero neighbours of frontier cells
getFrontier :: Grid -> ([Int], [Cell], [Int])
getFrontier grid = do
    -- get all unvrevealed cells
    let hiddenCells = map index $ V.toList $ V.filter (stateIs Hidden) (cells grid)
    -- partition by whether the cell is next to a revealed cell
    let (frontierIndexes, others) = partition hasRevealedNeighbour hiddenCells
    -- get all revealed cells that neighbour a frontier cell, filtering out duplicates
    let frontierNeighbours = nub . filter (stateIs Revealed) $ concatMap (getNeighbours grid) frontierIndexes
    (frontierIndexes, frontierNeighbours, others)
    where
        hasRevealedNeighbour index =
            -- get neighbours of a cell and return true if any are revealed
            let neighbours = getNeighbours grid index in
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
getProbableMove :: Int -> Frontier -> IO (Move, Int)
getProbableMove remainingBombs (frontierCells, neighbourCells, numOthers) =
    if length frontierCells > 32 && remainingBombs > 12 then return (Naive $ getNaiveGuess neighbourCells, 1)
    else do
        -- generate all possible valid arrangements using backtracking
        arrangements <- findValidArrangements frontierCells remainingBombs neighbourCells
        -- calculate probability of each cell containing a bomb
        let move = probToMove frontierCells $ calculateProbabilities arrangements remainingBombs numOthers
        return (move, minimum $ map length arrangements)


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
                partialRes <- newEmptyMVar
                -- new fork for branch that includes current
                forkIO $ do
                    res <- findValidArrs rest (remainingBombs-1) (current : currentArrangement) (remainingThreads-1)
                    putMVar partialRes res
                -- use current thread for branch that excludes current
                resExclude <- findValidArrs rest remainingBombs currentArrangement (remainingThreads-1)
                -- once other thread finishes, combine results
                resInclude <- takeMVar partialRes
                return (resInclude ++ resExclude)
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
calculateProbabilities arrangements remainingBombs numOthers =
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
probToMove :: [Int] -> [(Int, Rational)] -> Move
probToMove frontierCells probs = do
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
