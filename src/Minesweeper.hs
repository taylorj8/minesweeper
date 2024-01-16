module Minesweeper where

import Util

import Graphics.UI.Threepenny.Core (Element)
import System.Random (mkStdGen, randomRs)
import Data.List (nub, (\\))
import Data.Time (getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Data.Vector as V


-- randomly select numBombs indexes from a list numbers [0..n*n]
-- nub removes duplicates from the random number stream
-- safeCells can't have bombs - first cell revealed and its neighbours
-- ensures the grid always starts with the correct number of bombs
randSelect :: Int -> Int -> Int -> Int -> [Int]
randSelect n numBombs firstCell seed = take numBombs . nub . filter (`notElem` safeCells) $ randomRs (0, n*n-1) $ mkStdGen seed
    where 
        safeCells = firstCell : findNeighbours firstCell n

-- system time (converted to Int) used as seed for random number generator
sysTime :: IO Int
sysTime = do
    time <- getCurrentTime
    return $ floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds $ time


-- place bombs at the given indices
placeBombs :: Grid -> [Int] -> Grid
placeBombs (Grid n c cells) bombIndices = Grid n c (V.map placeBomb cells)
    where
        -- if cell's index is in bombIndices, place bomb
        placeBomb (Cell index square _ _) = if index `elem` bombIndices then bomb index square Hidden else empty index square Hidden 0


-- for each empty cell, count the number of surrounding bombs
countBombs :: Grid -> Grid
countBombs (Grid n c cells) = Grid n c (V.map count cells)
    where
        -- if cell is bomb, do nothing
        count (Cell i e r Bomb) = (Cell i e r Bomb)
        -- else count the bombs in surrounding cells
        count (Cell i e r (Empty _)) = empty i e r (countNeighbours n i cells)


-- get the neighbours of a cell and filter out the empty cells
-- to find the number of surrounding bombs
countNeighbours :: Int -> Int -> V.Vector Cell -> Int
countNeighbours n i cells = length $ filter isBomb neighbours
    where
        neighbours = findNeighbours i n
        isBomb index = cellType (cells V.! index) == Bomb


-- given an index, return the indices of the surrounding cells
-- only real disadvantage caused by using 1D representation
findNeighbours :: Int -> Int -> [Int]
findNeighbours index n = handleEdges [index - n-1, index - n, index - n+1, index - 1, index + 1, index + n-1, index + n, index + n+1]
    where
        handleEdges = filter (\x -> x >= 0 && x < n*n && abs (x `mod` n - index `mod` n) <= 1)
        -- handles the case of edge cells
        -- if neigbour is out of bounds, remove it
        -- if neighbour is more than one column away, remove it


-- reset the grid
-- get list of bomb positions, place the bombs and find the surrounding bomb count for empty cells
-- grid is initialised after first cell is revealed
-- index of first cell is passed to avoid placing a bomb there
resetGrid :: Int -> Int -> Int -> Grid -> Grid
resetGrid numBombs firstCell seed grid = countBombs $ placeBombs grid $ randSelect (size grid) numBombs firstCell seed

-- return a grid with all empty cells
-- used before first cell revealed, then resetGrid called
emptyGrid :: Int -> (Element, Element) -> [Element] -> Grid
emptyGrid size bar squares = Grid size bar $ V.fromList $ map (blankCell) (zip squares [0..])
    where
        blankCell (e, i) = Cell i e Hidden (Empty 0)

-- helper function to get cell from grid
getCell :: Grid -> Int -> Cell
getCell (Grid _ _ cells) index = cells V.! index


-- recursive function to reveal all 0 cells
-- maintains two lists - indexes of cells to reveal and indexes to be checked
-- if a cell contains 0, get its neighbours that haven't already been checked
-- add this to both lists, recurse until no more cells to check
-- the head of the indexes to be checked is removed with each recursion
revealIndexes :: Grid -> Int -> [Int]
revealIndexes grid index = revealIndexes' grid [index] [index]
    where
        revealIndexes' grid indexes [] = indexes
        revealIndexes' grid indexes (current : rest) = case (cells grid) V.! current of
            Cell _ _ _ (Empty 0) -> do
                let newNeighbours = (findNeighbours current (size grid)) \\ indexes
                revealIndexes' grid (indexes ++ newNeighbours) (rest ++ newNeighbours)
            otherwise -> revealIndexes' grid indexes rest


-- set cellState to Flagged if Hidden and vice versa
toggleFlagged :: Int -> Grid -> Grid
toggleFlagged index (Grid n c cells) = do
    let cell = cells V.! index
    case cellState cell of
        Hidden -> Grid n c (cells V.// [(index, cell { cellState = Flagged })])
        Flagged -> Grid n c (cells V.// [(index, cell { cellState = Hidden })])
        Revealed -> Grid n c cells


-- sets cell state of cells at given indexes to Revealed
-- counts how many were updated - needed to accurately determine win condition
updateCells :: [Int] -> Grid -> (Grid, Int)
updateCells indexes (Grid n c cells) = do
    -- for each index, get the associated cell and update it
    -- function returns a tuple, so unzip into two lists
    let (updatedCells, updated) = unzip $ map (update . getCell) indexes
    -- return a grid with the updated cells and sum the second list to get number of cells updated
    (Grid n c (cells V.// (zip indexes updatedCells)), sum updated)
    where 
        getCell i = cells V.! i
        update cell = case cellState cell of
            Hidden -> (cell { cellState = Revealed }, 1)
            _ -> (cell, 0)
