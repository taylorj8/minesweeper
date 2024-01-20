module Solver where

import Util
import Minesweeper

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (on)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Vector as V
import Data.List (partition, nub, groupBy, sortBy, minimumBy)
import Data.Function (on)
import Data.Ord (comparing, Down (Down))
import Data.Ratio ((%))
import Control.Concurrent (threadDelay)
import qualified Data.Set as S


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
                        success <- logicSolve gridRef stateRef currentRef probText
                        if success then return True
                        else do
                            -- if logical move can't be found, calculate probabilistic move
                            element probText # set UI.text "Calculating"
                            let probList = findProbableMove grid bombsRemaining
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
                                _ -> return False
                    -- uncertain/naive moves from last turn are taken here
                    -- also certain moves if more than one found
                    _ -> takeProbableMove gridRef stateRef probRef probText
        _ -> return False


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


-- repeatedly call solve until no remaining moves
autoSolve :: IORef Grid -> IORef GameState -> IORef Int -> IORef ProbableMove -> (Element, Element) -> UI ()
autoSolve gridRef stateRef currentRef probRef (probText, autoButton) = do
    -- highlight button while auto solver running
    element autoButton # set UI.style [("background-color", "LightGoldenRodYellow")]
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


-- return the middle index of the grid
-- top left middle in case of even grid
middleIndex :: Grid -> Int
middleIndex (Grid n _ _) = case n `mod` 2 of
    1 -> n * n `div` 2
    _ -> n * n `div` 2 - n `div` 2


-- try to find a move based on simple logic rules
-- any moves found are always safe
logicSolve :: IORef Grid -> IORef GameState -> IORef Int -> Element -> UI Bool
logicSolve gridRef stateRef currentRef probText = do
    current <- liftIO $ readIORef currentRef
    grid <- liftIO $ readIORef gridRef
    solveFlags' grid current 0
    where
        solveFlags' grid cur iterations = do
            -- mod keeps index inside grid
            let current = cur `mod` totalSize grid
            let cell = grid `getCell` current
            -- if no move found after checking every cell, return
            if iterations > totalSize grid then return False
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


-- check the probability list and perform the move inside
-- could be a safe or unsafe move
takeProbableMove :: IORef Grid -> IORef GameState -> IORef ProbableMove -> Element -> UI Bool
takeProbableMove gridRef stateRef probRef probText = do
    probList <- liftIO $ readIORef probRef
    case probList of
        -- contains list of guaranteed bombs and safe cells
        Certain guaranteedCells -> takeCertainMove gridRef stateRef probRef probText guaranteedCells
        -- both below contain the safest cell found from unsafe options
        -- click it and update IORef
        Uncertain (safest, prob) -> do
            clickCell safest gridRef stateRef probText
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
    (flag : rest, safes) -> do
        flagCell flag gridRef stateRef probText
        liftIO $ writeIORef probRef $ Certain (rest, safes)
        return True
    ([], safe : rest) -> do
        clickCell safe gridRef stateRef probText
        if null rest then liftIO $ writeIORef probRef None
        else liftIO $ writeIORef probRef $ Certain ([], rest)
        return True
    _ -> return False


-- find a probable move
findProbableMove :: Grid -> Int -> ProbableMove
findProbableMove grid bombsRemaining = do
    -- get the frontier cells, their neighbours and the number of remaining hidden cells
    let (frontierCells, frontierNeighbours, numOthers) = getFrontier grid
    let neighbourCells = convertCells frontierNeighbours grid frontierCells
    -- too many cells, generating all possible arrangments would take too long so make a naive guess
    if length frontierCells > 92 && bombsRemaining > 12 then Naive $ getNaiveGuess neighbourCells
    else do
        -- generate all possible valid arrangements using backtracking
        let arrangements = findValidArrangements frontierCells bombsRemaining neighbourCells
        -- calculate each cell containing a bomb
        toProbList frontierCells $ calculateProbabilities arrangements bombsRemaining numOthers


-- make a guess based on individual cells
-- not very accurate, but better than waiting 5 minutes for a guess
getNaiveGuess :: [NeighbourCell] -> (Int, Float)
getNaiveGuess cells = getSafest $ map checkCell cells
    where
        checkCell (num, neighbours) = (minimum neighbours, fromIntegral num / fromIntegral (length neighbours))


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


-- helper function to get the safest option
getSafest :: Ord a => [(Int, a)] -> (Int, a)
getSafest = minimumBy (comparing snd)


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
            (newNum, neighbours)
        convertCell _ = (0, S.empty)


-- return true if cell is in passed state
stateIs :: CellState -> Cell -> Bool
stateIs s c = cellState c == s


-- find all possible valid arrangements of bombs
-- uses backtracking to stop generating possibilities that contain an invalid subarrangement
-- result doesn't check that too few bombs have been placed, so filter out these cases
findValidArrangements :: [Int] -> Int -> [NeighbourCell] -> [Arrangement]
findValidArrangements availableCells remainingBombs frontierNeighbours = filter isValidArrangement $ findValidArrs availableCells remainingBombs []
    where
        findValidArrs :: [Int] -> Int -> Arrangement -> [Arrangement]
        findValidArrs [] _ currentArrangement = [currentArrangement]  -- stop when out of cells
        findValidArrs _ 0 currentArrangement = [currentArrangement]   -- stop when out of bombs
        -- for each available cell, it can either be included or excluded
        -- recursively find all other arrangements in each case and combine results
        findValidArrs (current : rest) remainingBombs currentArrangement
            | isValidSubArrangement currentArrangement =
                findValidArrs rest (remainingBombs-1) (current : currentArrangement) ++ findValidArrs rest remainingBombs currentArrangement
            | otherwise = findValidArrs rest remainingBombs currentArrangement
        -- count the number of bombs in an arrangement and compare to n of each cell
        -- a subarrangement is valid if too many bombs aren't placed beside any neighbour cell
        -- a whole arrangment is valid if the exact right number of bombs are placed by all neighbour cells
        isValidSubArrangement arrangement = all (isValid (>=) arrangement) frontierNeighbours
        isValidArrangement arrangement = all (isValid (==) arrangement) frontierNeighbours
        isValid c arrangement (n, neighbours) = n `c` length (filter (`S.member` neighbours) arrangement)


-- calculate the probabilities of each cell from the valid arrangements
-- returns index mapped to probability
-- Integer and Rational used to avoid overflow
calculateProbabilities :: [[Int]] -> Int -> Int -> [(Int, Rational)]
calculateProbabilities arrangements remainingBombs numOthers = do
    -- for each arrangement, get the number of ways to arrange the remaining bombs
    let counts = map (ch . length) arrangements
    let totalArrangements = sum counts
    -- count number of time cell contains a bomb in every possible arrangement
    let weightedCounts = weightedCount arrangements counts
    -- divide by the number of possible arrangments to get a probability for each cell
    map (\(i, prob) -> (i, prob % totalArrangements)) weightedCounts
    -- find any cells that don't appear in any arrangement and add them with probability 0
    -- let safeCells = filter (`notElem` map fst cellProbs) frontierCells
    -- cellProbs ++ zip safeCells (repeat 0.0)
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
