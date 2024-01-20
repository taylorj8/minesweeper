module Minesweeper where

import Util

import Graphics.UI.Threepenny.Core hiding (empty)
import System.Random (mkStdGen, randomRs)
import Data.List (nub, (\\))
import Data.Time (getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Data.Vector as V

import Control.Monad (when)
import Data.IORef (IORef, readIORef, writeIORef, modifyIORef)
import qualified Graphics.UI.Threepenny as UI



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
countBombs grid = grid { cells = (V.map count (cells grid)) }
    where
        -- if cell is bomb, do nothing
        count (Cell i e r Bomb) = (Cell i e r Bomb)
        -- else count the bombs in surrounding cells
        count (Cell i e r (Empty _)) = empty i e r (countNeighbours i grid)


-- get the neighbours of a cell and filter out the empty cells
-- to find the number of surrounding bombs
countNeighbours :: Int -> Grid -> Int
countNeighbours i (Grid n _ cells) = length $ filter isBomb neighbours
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


-- get the cells themselves
getNeighbours :: Grid -> Int -> [Cell]
getNeighbours grid index = map (getCell grid) $ findNeighbours index (size grid)


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



-- toggleFlagged :: Int -> Grid -> Grid
-- toggleFlagged index (Grid n c cells) = do
--     let cell = cells V.! index
--     case cellState cell of
--         Hidden -> Grid n c (cells V.// [(index, cell { cellState = Flagged })])
--         Flagged -> Grid n c (cells V.// [(index, cell { cellState = Hidden })])
--         Revealed -> Grid n c cells


-- set cellState to Flagged if Hidden and vice versa for indices given
toggleFlagged :: Int -> Grid -> Grid
toggleFlagged index (Grid n c cells) = Grid n c (cells V.// [(index, toggle index)])
    where
        getCell i = cells V.! i
        toggle i = let cell = cells V.! i in
            case cellState cell of
                Hidden -> cell { cellState = Flagged }
                Flagged -> cell { cellState = Hidden }
                Revealed -> cell


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


-- handles left click
clickCell :: Int -> IORef Grid -> IORef GameState -> Element -> UI ()
clickCell index gridRef stateRef probText = do
    element probText # set UI.text ""
    gameState <- liftIO $ readIORef stateRef
    case gameState of
        -- when game not yet started
        -- updates game state and resets the grid
        -- then reveals cells
        GameStart numBombs -> do
            (Grid n _ _) <- liftIO $ readIORef gridRef
            liftIO $ writeIORef stateRef $ Playing (n*n - numBombs, numBombs)
            seed <- liftIO sysTime
            -- liftIO $ print seed
            liftIO $ modifyIORef gridRef $ resetGrid numBombs index 1705754070
            revealCells index gridRef stateRef
        -- when in game
        -- if hidden cell clicked on, reveal cells
        Playing _ -> do
            grid <- liftIO $ readIORef gridRef
            when (cellState (grid `getCell` index) == Hidden) $ revealCells index gridRef stateRef
        _ -> return ()


-- handles internal and UI changes for all affected cells
-- also handles win condition
revealCells :: Int -> IORef Grid -> IORef GameState -> UI ()
revealCells index gridRef stateRef = do
    grid <- liftIO $ readIORef gridRef
    -- get indexes of cells to be revealed
    let indexes = revealIndexes grid index
    -- update the cells, returning the number of cells updated
    let (grid', cellsRevealed) = updateCells indexes grid
    -- handles win condition
    trackRemainingCells grid stateRef cellsRevealed
    -- update the UI
    mapM_ (revealCell grid stateRef) indexes
    liftIO $ writeIORef gridRef grid'
    return ()


-- updates the UI for a single cell
-- also handles lose condition
revealCell :: Grid -> IORef GameState -> Int -> UI Element
revealCell grid stateRef index = do
    -- let temp = cells grid V.!? index
    -- when (isMaybe temp) liftIO $ print
    let (Cell _ square state typ) = cells grid V.! index
    case state of
        -- cell should only be updated if it is currently hidden
        Hidden -> do
            case typ of
                -- if bomb revealed, game over
                Bomb -> do
                    fillBombs GameOver (cells grid)  -- reveal all bombs
                    liftIO $ writeIORef stateRef GameOver  -- update game state
                    element square # set UI.style [("background-color", "red")]  -- bomb chosen should show red
                    element (fst $ top grid) # set UI.text "Game Over"  -- update title text
                -- if cell empty, reveal and show number (or blank if 0)
                (Empty numBombs) -> element square
                    # set UI.style [("background-color", "whitesmoke"), ("color", textColor numBombs)]
                    # set UI.text (show typ)
        _ -> return square


-- keeps track of how many cells remain and updates UI on win
trackRemainingCells :: Grid -> IORef GameState -> Int -> UI ()
trackRemainingCells grid stateRef num = do
    state <- liftIO $ readIORef stateRef
    let state' = trackRemainingCells' num state
    -- on win, fill bomb cells with flags and update title and flag number
    when (state' == Win) $ do
        fillBombs Win (cells grid)
        element (fst $ top grid) # set UI.text "You Win!"
        element (snd $ top grid) # set UI.text "0"
        return ()
    liftIO $ writeIORef stateRef state'  -- write back state
        where
            -- subtract number of cells revealed from remaining total
            -- if remaining == 0, game is won
            trackRemainingCells' num (Playing (t, c)) = case t-num of
                0 -> Win
                _ -> Playing (t-num, c)
            trackRemainingCells' _ state = state


-- handles right click
flagCell :: Int -> IORef Grid -> IORef GameState -> Element -> UI ()
flagCell index gridRef stateRef probText = do
    element probText # set UI.text ""
    gameState <- liftIO $ readIORef stateRef
    case gameState of
        -- change should only happen when in game
        Playing (_, numBombs) -> do
            grid <- liftIO $ readIORef gridRef
            let cell = cells grid V.! index
            -- internally toggle the cell's flag
            liftIO $ writeIORef gridRef $ toggleFlagged index grid
            case cellState cell of
                -- update the UI and update the flag count
                Hidden -> do
                    liftIO $ modifyIORef stateRef $ updateCounter (-)
                    element (snd $ top grid) # set UI.text (show $ numBombs-1)
                    element (square cell) # set UI.text "🚩"
                Flagged -> do
                    liftIO $ modifyIORef stateRef $ updateCounter (+)
                    element (snd $ top grid) # set UI.text (show $ numBombs+1)
                    element (square cell) # set UI.text ""
                _ -> element (square cell)
            return ()
            where
                -- add/subtract 1 from counter
                updateCounter op (Playing (t, c)) = Playing (t, c `op` 1)
                updateCounter _ s = s
        _ -> return ()


-- restarts the game
handleRestart :: IORef Grid -> IORef GameState -> IORef Int -> IORef ProbableMove -> Element -> Int -> UI ()
handleRestart gridRef stateRef solveRef probRef probText numBombs = do
    Grid _ top cells <- liftIO $ readIORef gridRef
    resetTopBar top  -- reset title and flag count
    V.mapM_ resetSquare cells  -- reset UI
    -- reset IORefs
    element probText # set UI.text ""
    liftIO $ writeIORef stateRef (GameStart numBombs) 
    liftIO $ modifyIORef gridRef $ \grid -> grid { cells = V.map resetCell cells }
    liftIO $ writeIORef solveRef 0
    liftIO $ writeIORef probRef None
        where
            -- set cell as hidden and empty
            resetCell (Cell index square _ _) = Cell index square Hidden (Empty 0)
            -- remove text and set color back to grey
            resetSquare (Cell _ square _ _) = element square
                # set UI.style [("background-color", "lightgrey")]
                # set UI.text ""
            -- reset top bar text
            resetTopBar (title, counter) = do
                element title # set UI.text "Minesweeper"
                element counter # set UI.text (show numBombs)


-- update the UI of all bomb cells depending on game state
fillBombs :: GameState -> V.Vector Cell -> UI ()
fillBombs state = V.mapM_ (fillBomb state)
    where
        fillBomb state (Cell _ square _ typ) = do
            case (state, typ) of
                -- in case of Win, add a flag
                (Win, Bomb) -> element square # set UI.text "🚩"
                -- in the case of GameOver, reveal
                (GameOver, Bomb) -> element square
                    # set UI.style [("background-color", "whitesmoke")]
                    # set UI.text "💣"
                _ -> return square


-- return a different color based on the number of bombs
textColor :: Int -> String
textColor n = case n of
    1 -> "blue"
    2 -> "green"
    3 -> "red"
    4 -> "purple"
    5 -> "maroon"
    6 -> "turquoise"
    7 -> "black"
    8 -> "grey"
    _ -> "whitesmoke"
