module Util where

import qualified Data.Vector as V
import qualified Data.Set as S
import Graphics.UI.Threepenny.Core (Element)

-- tracks the state of the game
-- allows click behaviour to be changed based on state
-- GameStart state contains the number of bombs
-- Playing state contains number of hidden non-bomb cells and number of remaining flags
data GameState
    = GameStart Int
    | Playing (Int, Int)
    | GameOver
    | Win
    deriving Eq

-- cell can either be a bomb or a number representing surrounding bombs
data CellType
    = Bomb
    | Empty Int
    deriving Eq

-- cell can be hidden, revealed or flagged
data CellState
    = Hidden
    | Revealed
    | Flagged
    deriving (Show, Eq)

-- all data cell needs
-- index, UI representation, state and type
data Cell = Cell {
    index :: Int,
    square :: Element,
    cellState :: CellState,
    cellType :: CellType
}

-- helper functions to create a Cell
bomb :: Int -> Element -> CellState -> Cell
bomb i e r = Cell i e r Bomb

empty :: Int -> Element -> CellState -> Int -> Cell
empty i e r n = Cell i e r (Empty n)

-- cells are equal if their indexes are equal
instance Eq Cell where
    c1 == c2 = index c1 == index c2

-- show B for bomb or number in empty cell
instance Show Cell where
    show (Cell index _ state (Empty n)) = show (index, n)
    show (Cell index _ _ _) = show (index, -1)

instance Show CellType where
    show Bomb = "B"
    show (Empty 0) = " "
    show (Empty n) = show n

-- board represented as a vector of cells
-- size of rows, UI elements representing top bar, vector of cells
-- a vector is used as accessing cells by index is needed frequently
-- I chose a 1D representation rather than 2D - explanation in report
data Grid = Grid {
    size :: Int,
    top :: (Element, Element),
    cells :: V.Vector Cell
}

-- show grid in 2D format
instance Show Grid where
    show (Grid n _ cells) = unlines $ map showRow rows
        where
            rows = [take n $ drop (i*n) $ V.toList cells | i <- [0..n-1]]
            showRow = unwords . map show

-- grid helper functions
getCell :: Grid -> Int -> Cell
getCell (Grid _ _ cells) index = cells V.! index

-- total number of cells in the grid
totalSize :: Grid -> Int
totalSize (Grid n _ _) = n*n

 -- either contains or unsafe options
 -- Certain has list of guaranteed bombs to flag and guaranteed safe cells
 -- Uncertain and Naive contain cell with lowest probability of a bomb
 -- Naive may be inaccurate
data ProbableMove
    = Certain ([Int], [Int])
    | Uncertain (Int, Rational)
    | Naive (Int, Float)
    | None
    deriving Show

-- contains number of unflagged bombs and set of neighbouring frontier cells
-- set used for O(logN) lookup
type NeighbourCell = (Int, S.Set Int)

-- possible arrangement of bombs
type Arrangement = [Int]

data Direction 
    = North
    | East
    | South
    | West
    | Apart
    deriving (Eq, Ord, Enum)