module Minesweeper where

import Graphics.UI.Threepenny.Core (Element)
import System.Random (mkStdGen, randomRs)
import Data.List (nub, (\\))
import Data.Time (getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Data.Vector as V

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
    top :: (Element, Element),
    cells :: (V.Vector Cell)
}


-- show grid in 2D format
instance Show Grid where
    show (Grid n _ cells) = unlines $ map showRow rows
        where
            rows = [take n $ drop (i*n) $ V.toList cells | i <- [0..n-1]]
            showRow = unwords . map show


-- randomly select numBombs indexes from a list of size n*n for placing bombs
-- nub removes duplicates from the random number stream
-- safeCells can't have bombs - first cell revealed and its neighbours
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
        placeBomb (Cell index square _ _) = if index `elem` bombIndices then bomb index square Hidden else empty index square Hidden 0


-- for each empty cell, count the number of surrounding bombs
countBombs :: Grid -> Grid
countBombs (Grid n c cells) = Grid n c (V.map count cells)
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
resetGrid :: Int -> Int -> Int -> Grid -> Grid
resetGrid numBombs firstCell seed grid = countBombs $ placeBombs grid $ randSelect (size grid) numBombs firstCell seed

emptyGrid :: Int -> (Element, Element) -> [Element] -> Grid
emptyGrid size bar squares = Grid size bar $ V.fromList $ map (blankCell) (zip squares [0..])

blankCell :: (Element, Int) -> Cell
blankCell (e, i) = Cell i e Hidden (Empty 0)

getCell :: Grid -> Int -> Cell
getCell (Grid _ _ cells) index = cells V.! index


-- recursive function to reveal all 0 cells
-- keeps track of two lists - indexes of cells to reveal and indexes to be checked
-- if a cell contains 0, get its neighbours that haven't already been checked
-- add this to both lists, recurse until no more cells to check
revealIndexes :: Grid -> Int -> [Int]
revealIndexes grid index = revealIndexes' grid [index] [index]
    where
        revealIndexes' grid indexes [] = indexes
        revealIndexes' grid indexes (current : rest) = case (cells grid) V.! current of
            Cell _ _ _ (Empty 0) -> do
                let newNeighbours = (findNeighbours current (size grid)) \\ indexes
                revealIndexes' grid (indexes ++ newNeighbours) (rest ++ newNeighbours)
            otherwise -> revealIndexes' grid indexes rest


toggleFlagged :: Int -> Grid -> Grid
toggleFlagged index (Grid n c cells) = do
    let cell = cells V.! index
    case cellState cell of
        Hidden -> Grid n c (cells V.// [(index, cell { cellState = Flagged })])
        Flagged -> Grid n c (cells V.// [(index, cell { cellState = Hidden })])
        Revealed -> Grid n c cells

-- returns updated Grid along with number of cells revealed
updateCells :: [Int] -> Grid -> (Grid, Int)
updateCells indexes (Grid n c cells) = do
    let updatedCells = map updateAndGetCount $ zip indexes (map getCell indexes)
        cells' = map fst updatedCells
        hiddenCount = sum (map snd updatedCells)
    (Grid n c (cells V.// (zip indexes cells')), hiddenCount)
    where 
        getCell i = cells V.! i
        updateAndGetCount (i, cell) = case cellState cell of
            Hidden -> (cell { cellState = Revealed }, 1)
            _ -> (cell, 0)