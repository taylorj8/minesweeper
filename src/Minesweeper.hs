module Minesweeper where

{-# LANGUAGE BangPatterns #-}

import System.Random (mkStdGen, randomRs)
import Data.List (nub, (\\), concatMap)
import Data.Time (getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.IO.Unsafe (unsafePerformIO)
import System.Random.Shuffle (shuffle')
import qualified Data.Vector as V

import Graphics.UI.Threepenny.Core (Element)

-- cell can either be a bomb or a number representing surrounding bombs
-- also stores the index of the cell
data CellType
    = Bomb
    | Empty Int
    deriving Eq

data CellState
    = Hidden
    | Revealed
    | Flagged
    deriving (Show, Eq)

data Cell = Cell {
    index :: Int,
    square :: Element,
    cellState :: CellState,
    cellType :: CellType
}

bomb :: Int -> Element -> CellState -> Cell
bomb i e r = Cell i e r Bomb

empty :: Int -> Element -> CellState -> Int -> Cell
empty i e r n = Cell i e r (Empty n)

-- show B for bomb or number in empty cell
instance Show Cell where
    show (Cell _ _ _ typ) = show typ

instance Show CellType where
    show Bomb = "B"
    show (Empty 0) = " "
    show (Empty n) = show n

-- board represented as a vector of cells
-- a vector is used as accessing cells by index is needed frequently
data Grid = Grid {
    size :: Int,
    cells :: (V.Vector Cell)
}


-- show grid in 2D format
instance Show Grid where
    show (Grid n cells) = unlines $ map showRow rows
        where
            rows = [take n $ drop (i*n) $ V.toList cells | i <- [0..n-1]]
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
placeBombs :: Grid -> [Int] -> Grid
placeBombs (Grid n cells) bombIndices = Grid n (V.map placeBomb cells)
    where
        placeBomb (Cell index square _ _) = if index `elem` bombIndices then bomb index square Hidden else empty index square Hidden 0


-- for each empty cell, count the number of surrounding bombs
countBombs :: Grid -> Grid
countBombs (Grid n cells) = Grid n (V.map count cells)
    where
        count (Cell i e r Bomb) = (Cell i e r Bomb)
        count (Cell i e r (Empty _)) = empty i e r (countNeighbours n i cells)


-- find the neighbours of a cell and filter out the empty cells
-- to find the number of surrounding bombs
countNeighbours :: Int -> Int -> V.Vector Cell -> Int
countNeighbours n i cells = length $ filter isBomb neighbours
    where
        neighbours = findNeighbours i n
        isBomb index = cellType (cells V.! index) == Bomb


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
resetGrid :: Int -> Int -> Grid -> Grid
resetGrid numBombs firstCell grid = countBombs $ placeBombs grid $ randSelect' (size grid) numBombs firstCell

emptyGrid :: Int -> [Element] -> Grid
emptyGrid size squares = Grid size $ V.fromList $ map (blankCell) (zip squares [0..])

blankCell :: (Element, Int) -> Cell
blankCell (e, i) = Cell i e Hidden (Empty 0)

-- blockReveal :: Int -> Grid -> Grid
-- blockReveal index grid = revealCells (blockReveal' grid index) grid

-- recursive function to reveal all 0 cells
-- if a cell contains 0, recurse to neighbours
-- concatenate results to get the indices of cells to be revealed
blockReveal :: Grid -> Int -> [Int]
blockReveal grid index = blockReveal' grid [] index
    where
        blockReveal' grid visited index
            | index `elem` visited = []
            | otherwise = case (cells grid) V.! index of
                Cell _ _ _ (Empty 0) -> do
                    let neighbours = findNeighbours index (size grid)
                    index : (concatMap (blockReveal' grid (index : visited)) neighbours)
                otherwise -> [index]


toggleFlagged :: Int -> Grid -> Grid
toggleFlagged index (Grid n cells) = do
    let cell = cells V.! index
    case cellState cell of
        Hidden -> Grid n (cells V.// [(index, cell { cellState = Flagged })])
        Flagged -> Grid n (cells V.// [(index, cell { cellState = Hidden })])
        Revealed -> Grid n cells

updateCells :: [Int] -> Grid -> Grid
updateCells indexes (Grid n cells) = do
    let cells' = map (update . getCell) indexes
    Grid n (cells V.// (zip indexes cells'))
        where 
            getCell i = cells V.! i
            update cell = case cellState cell of
                Hidden -> cell { cellState = Revealed }
                _ -> cell

    
-- reveals the cells at the given indices
-- revealCells :: [Int] -> Grid -> Grid
-- revealCells indexes (Grid n cells) = Grid n (map reveal cells)
--     where
--         reveal (i, r, Bomb) = if i `elem` indexes then bomb i True else bomb i r
--         reveal (i, r, (Empty n)) = if i `elem` indexes then empty i True n else empty i r n