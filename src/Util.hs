module Util where

import qualified Data.Vector as V
import qualified Data.Set as S
import Graphics.UI.Threepenny.Core (Element)
import Data.List (union)

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

-- update the gamestate with the new number of bombs
updateState :: Int -> GameState -> GameState
updateState numBombs gameState = do
    case gameState of
        GameStart _ -> GameStart numBombs
        _ -> gameState

-- return true if cell is in passed state
stateIs :: CellState -> Cell -> Bool
stateIs s c = cellState c == s


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

-- update the size and squares of the grid
updateGrid :: Int -> [Element] -> Grid -> Grid
updateGrid size squares grid = do
    let newCells = V.fromList $ zipWith (curry updateSquare) squares [0..size*size]
    grid { size = size, cells = newCells }
    where
        updateSquare (square, index) = Cell index square Hidden (Empty 0)

-- grid helper functions
getCell :: Grid -> Int -> Cell
getCell (Grid _ _ cells) index = cells V.! index

-- total number of cells in the grid
totalSize :: Grid -> Int
totalSize (Grid n _ _) = n*n

-- return the middle index of the grid
-- top left middle in case of even grid
middleIndex :: Grid -> Int
middleIndex (Grid n _ _) = case n `mod` 2 of
    1 -> n * n `div` 2
    _ -> n * n `div` 2 - n `div` 2


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

-- determines how to combine probable moves
-- given two Certains, union their lists
-- given Certain and any other, take the certain
-- given two Uncertains, take the lowest probability
-- given Uncertain and anything else, take Uncertain
-- Naive is similar to Uncertain
-- given none, always take other option
combineProbs :: [ProbableMove] -> ProbableMove
combineProbs [] = None
combineProbs (head : rest) = foldl concatProbableMoves' head rest
    where
        concatProbableMoves' m1 m2 = case (m1, m2) of
            (Certain (b1, s1), Certain (b2, s2)) -> Certain ( b1 `union` b2,  s1 `union` s2)
            (Certain x, _) -> Certain x
            (Uncertain (i1, p1), Uncertain (i2, p2)) -> 
                if p1 < p1 then Uncertain (i1, p1)
                else Uncertain (i2, p2)
            (Uncertain _, Certain x) -> Certain x
            (Uncertain x, _) -> Uncertain x
            (Naive (i1, p1), Naive (i2, p2)) -> 
                if p1 < p1 then Naive (i1, p1)
                else Naive (i2, p2)
            (Naive x, None) -> Naive x
            (Naive _, x) -> x
            (None, x) -> x


-- contains number of unflagged bombs and set of neighbouring frontier cells
-- set used for O(logN) lookup
type NeighbourCell = (Int, S.Set Int)

-- possible arrangement of bombs
type Arrangement = [Int]

-- represents a frontier between hidden and revealed cells
-- [Int] represents hidden cells on frontier
-- [NeighbourCell] represents revealed cells on frontier
-- Int is number of hidden cells not on the frontier
type Frontier = ([Int], [NeighbourCell], Int)


-- contains the grid size and number of bombs
data Difficulty
    = Easy (Int, Int)
    | Medium (Int, Int)
    | Hard (Int, Int)

instance Show Difficulty where
    show (Easy _) = "Easy"
    show (Medium _) = "Medium"
    show (Hard _) = "Hard"

-- cycle through difficulties
change :: Difficulty -> Difficulty
change difficulty = case difficulty of
    Easy _ -> Medium (16, 40)
    Medium _ -> Hard (22, 99)
    Hard _ -> Easy (9, 10)

-- get the difficulty parameters
getParams :: Difficulty -> (Int, Int)
getParams difficulty = case difficulty of
    Easy params -> params
    Medium params -> params
    Hard params -> params

-- get the color of the button at each difficulty
getColor :: Difficulty -> String
getColor difficulty = case difficulty of 
    Easy _ -> "palegreen"
    Medium _ -> "PaleTurquoise"
    Hard _ -> "lightcoral"

-- get first element of 3-tuple
getFst :: (a, b, c) -> a
getFst (a, _, _) = a
