module Solver where

import Util
import Minesweeper

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (on)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import qualified Data.Vector as V
import Control.Monad (forever, unless, when, filterM)
import Data.List (subsequences, partition, nub, groupBy, sortBy, minimumBy)
import Data.Function (on)
import Data.Ord (comparing)
import Data.Ratio ((%))
import Control.Concurrent (threadDelay)


-- figures out next move and performs it if completely safe
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
                                Certain (flag : rest) -> do
                                    element probText # set UI.text ""
                                    flagCell flag gridRef stateRef probText
                                    if null rest then liftIO $ writeIORef probRef None
                                    else liftIO $ writeIORef probRef $ Certain rest
                                    return True
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
autoSolve :: IORef Grid -> IORef GameState -> IORef Int -> IORef ProbabilityList -> (Element, Element) -> UI ()
autoSolve gridRef stateRef currentRef probRef (probText, autoButton) = do
    -- highlight button while auto solver running
    element autoButton # set UI.style [("background-color", "LightGoldenRodYellow")]
    autoSolve' gridRef stateRef currentRef probRef
    where
        autoSolve' gridRef stateRef currentRef probRef = do
            -- repeat unless false returned, i.e. move not taken
            -- allows user to choose whether to take chance
            continue <- solve gridRef stateRef currentRef probRef probText
            if continue then do
                liftIO $ threadDelay 10000  -- delay for dramatic effect
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
takeProbableMove :: IORef Grid -> IORef GameState -> IORef ProbabilityList -> Element -> UI Bool
takeProbableMove gridRef stateRef probRef probText = do
    probList <- liftIO $ readIORef probRef
    case probList of
        -- contains list of guaranteed bombs
        Certain (flag : rest) -> do
            -- flag first in list then update IORef with remaining
            flagCell flag gridRef stateRef probText
            if null rest then liftIO $ writeIORef probRef None
            else liftIO $ writeIORef probRef $ Certain rest
            return True
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


-- find a probable move
findProbableMove :: Grid -> Int -> ProbabilityList
findProbableMove grid bombsRemaining = do
    let (frontierCells, frontierNeighbours, numOthers) = getFrontier grid
    let neighbourCells = convertCells frontierNeighbours grid frontierCells
    -- if too many posibilities, make a naive guess instead
    if numPosibilities frontierCells > 50000000 then Naive $ getNaiveGuess neighbourCells
    else do
        -- generate all possible arrangements of bombs in the remaining cells
        let arrangements = generateArrangements frontierCells bombsRemaining
        -- filter out arrangements that break the logic rules
        let validArrangements = checkArrangements neighbourCells arrangements
        -- calculate each cell containing a bomb
        toProbList $ calculateProbabilities validArrangements bombsRemaining numOthers
    where
        numPosibilities cells = sum (map (choose (length cells)) [1..(min bombsRemaining (length cells))])


-- make a guess based on individual cells
-- not very accurate, but better than waiting 5 minutes for a guess
getNaiveGuess :: [NeighbourCell] -> (Int, Float)
getNaiveGuess cells = getSafest $ map checkCell cells
    where
        checkCell (num, neighbours) = (head neighbours, fromIntegral num / fromIntegral (length neighbours))


-- takes in list of indexes to probabilities
-- partition out the guaranteed bombs
toProbList :: [(Int, Rational)] -> ProbabilityList
toProbList probs = case partition ((==1.0) . snd) probs of
        ([], []) -> None
        -- if no guaranteed bomb, choose the index with the lowest probability
        ([], uncertain) -> Uncertain $ getSafest uncertain
        -- otherwise return all guaranteed bombs to be flagged
        (bombs, _) -> Certain (map fst bombs)


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
            let neighbours = filter (`elem` frontierCells) neighbourIndexes
            (newNum, neighbours)
        convertCell _ = (0, [])


-- return true if cell is in passed state
stateIs :: CellState -> Cell -> Bool
stateIs s c = cellState c == s


-- generate all possible arrangements of bombs using subsequences
-- first arrangement can be dropped as it's always the empty list
-- if number of bombs is less than number of available cells, filter out arrangements too long
generateArrangements :: [Int] -> Int -> [[Int]]
generateArrangements availableCells numBombs
    | length availableCells <= numBombs = subseqs
    | otherwise = filter (\s -> length s <= numBombs) subseqs
    where subseqs = drop 1 $ subsequences availableCells


-- take all possible arrangements and filter out invalid arrangements
checkArrangements :: [NeighbourCell] -> [[Int]] -> [[Int]]
checkArrangements frontierNeighbours = filter isValidArrangement
    where
        -- an arrangement is valid if all the frontier neighbours' rules are followed
        isValidArrangement arrangement = all (isValid arrangement) frontierNeighbours
        -- count the number of bombs in an arrangement and compare to n
        -- arrangement is valid according to a given cell if these are equal
        isValid arrangement (n, neighbours) = n == length (filter (`elem` arrangement) neighbours)
                

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
