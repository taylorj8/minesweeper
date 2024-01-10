module Old where

import System.Random

main :: IO ()
main = do
    populateBoard $ initialiseBoard 10

-- cell is -1 for bomb, else number of surrounding bombs
-- bool represents if cell is revealed or not
-- first int is index of cell
data Cell = Cell (Int, Int) Int Bool
    deriving (Show)

data Board = Board Int [[Cell]] 

initialiseBoard :: Int -> Board
initialiseBoard size = Board size rows
    where
        rows = [[Cell index 0 False | index <- [0..size-1]] | _ <- [0..size-1]]


populateBoard :: Board -> IO Board
populateBoard (Board size rows) = do
    gen <- newStdGen
    let bombIndices = take size $ randomRs (0, size-1) gen
        updatedRows = map (populateRow bombIndices) rows
    return (Board size updatedRows)

populateRow :: [Int] -> [Cell] -> [Cell]
populateRow bombIndices cells = map (populateCell bombIndices) cells

populateCell :: [Int] -> Cell -> Cell
populateCell bombIndices (Cell index _ _) = if isBomb then Cell (-1) False else Cell numBombs False
    where
        isBomb = index `elem` bombIndices
        numBombs = countBombs bombIndices index

countBombs :: [Int] -> Int -> Int
countBombs bombIndices index = length $ filter (\i -> abs (i - index) == 1) bombIndices
