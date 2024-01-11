module Minesweeper where

{-# LANGUAGE BangPatterns #-}

import System.Random (mkStdGen, randomRs)
import Data.List (nub, (\\))
import Data.Time (getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.IO.Unsafe (unsafePerformIO)
import System.Random.Shuffle (shuffle')

-- cell can either be a bomb or a number representing surrounding bombs
-- also stores the index of the cell
data Cell
    = Bomb Int Bool
    | Empty Int Bool Int 

type Test = (Int, Bool, Cell)

-- show B for bomb or number in empty cell
instance Show Cell where
    show (Bomb _ _) = "B"
    show (Empty _ _ n) = show n

-- board represented as a list of cells
data Grid = Grid Int [Cell]

-- show grid in 2D format
instance Show Grid where
    show (Grid n cells) = unlines $ map showRow rows
        where
            rows = [take n $ drop (i*n) cells | i <- [0..n-1]]
            showRow = unwords . map show


-- BROKEN - random.shuffle not working with ThreePenny
-- randomly select numBombs indexes from a list of size n*n for placing bombs
-- generate list with all potential indexes, remove safe cells, randomly shuffle and select first numBombs
-- safeCells can't have bombs - first cell revealed and its neighbours
randSelect :: Int -> Int -> Int -> [Int]
randSelect n numBombs firstCell = take numBombs potentialCells
    where
        potentialCells = shuffle' ([0..n*n-1] \\ safeCells) (n*n-1) (mkStdGen timeSinceEpoch)
        safeCells = firstCell : findNeighbours firstCell n
        timeSinceEpoch = floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds $ unsafePerformIO getCurrentTime
        -- system time (converted to Int) used as seed for random number generator
        -- unsafePerformIO used to avoid returning IO Int


-- randomly select numBombs indexes from a list of size n*n for placing bombs
-- nub removes duplicates from the random number stream
-- safeCells can't have bombs - first cell revealed and its neighbours
randSelect' :: Int -> Int -> Int -> [Int]
randSelect' n numBombs firstCell = take numBombs . nub . filter (`notElem` safeCells) $ randomRs (0, n*n-1) $ mkStdGen nanosSinceEpoch
    where 
        safeCells = firstCell : findNeighbours firstCell n
        nanosSinceEpoch = floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds $ unsafePerformIO getCurrentTime
        -- system time (converted to Int) used as seed for random number generator
        -- unsafePerformIO used to avoid returning IO Int


-- place bombs at the given indices
placeBombs :: Int -> [Int] -> Grid
placeBombs n bombIndices = Grid n (map placeBomb [0..n*n])
    where
        placeBomb index = if index `elem` bombIndices then Bomb index False else Empty index False 0


-- for each empty cell, count the number of surrounding bombs
countBombs :: Grid -> Grid
countBombs (Grid n cells) = Grid n (map count cells)
    where
        count (Bomb r i) = Bomb r i
        count (Empty i r _) = Empty i r (countNeighbours n i cells)


-- find the neighbours of a cell and filter out the empty cells
-- to find the number of surrounding bombs
countNeighbours :: Int -> Int -> [Cell] -> Int
countNeighbours n i cells = length $ filter isBomb neighbours
    where
        neighbours = findNeighbours i n
        isBomb index = case cells !! index of
            Bomb _ _ -> True
            _ -> False


-- given an index, return the indices of the surrounding cells
findNeighbours :: Int -> Int -> [Int]
findNeighbours index n = handleEdges [index - n-1, index - n, index - n+1, index - 1, index + 1, index + n-1, index + n, index + n+1]
    where
        handleEdges = filter (\x -> x >= 0 && x < n*n && abs (x `mod` n - index `mod` n) <= 1)
        -- handles the case of edge cells
        -- if neigbour is out of bounds, remove it
        -- if neighbour is more than one column away, remove it


-- initialise the grid
-- get list of bomb positions, place the bombs and find the surrounding bomb count for empty cells
-- grid is initialised after first cell is revealed
-- index of first cell is passed to avoid placing a bomb there
initGrid :: Int -> Int -> Int -> Grid
initGrid size numBombs firstCell = countBombs $ placeBombs size $ randSelect' size numBombs firstCell


revealCell :: Int -> Grid -> Grid
revealCell index (Grid n cells) = Grid n (map reveal cells)
    where
        reveal (Bomb i r) = if i == index then Bomb i True else Bomb i r
        reveal (Empty i r n) = if i == index then Empty i True n else Empty i r n
