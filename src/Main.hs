module Main where

import Minesweeper
import Components

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C

import qualified Data.Vector as V
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Control.Monad (replicateM, when)


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


main :: IO ()
main = do
    startGUI defaultConfig setup


setup :: Window -> UI ()
setup window = do
    return window # set title "Minesweeper"
    -- stops context menu appearing when attempting to place flags
    runFunction $ ffi "window.oncontextmenu = function() { return false; }"

    -- board parameters
    let size = 16
    let numBombs = 40

    -- set up the grid
    -- contains size of grid, cells and top bar elements
    -- restart button left out as it is never changed
    title <- makeTitle
    bombCounter <- topCell (show numBombs)
    squares <- replicateM (size*size) uiCell
    gridRef <- liftIO $ newIORef $ emptyGrid size (title, bombCounter) squares

    -- initialise state and set onclick functions for the cells
    stateRef <- liftIO $ newIORef $ GameStart numBombs
    setOnClick gridRef stateRef

    -- set up last button and arrange in a row
    restartButton <- topCell "↺"
    on UI.click restartButton $ \_ -> handleRestart gridRef stateRef numBombs
    topBar <- UI.row [element bombCounter, element title, element restartButton]

    getBody window #+
        [
            UI.div #+ [element topBar # set UI.style [("margin", "auto")]],
            displayGrid squares size
        ]
        
    return ()


-- set up click handlers for each cell
-- left click to reveal cell, right click to place flag
setOnClick :: IORef Grid -> IORef GameState -> UI ()
setOnClick gridRef stateRef = do
    (Grid _ _ cells) <- liftIO $ readIORef gridRef
    mapM_ (clickHandlers gridRef) cells
        where
            clickHandlers gridRef (Cell i square state _) = do
                on UI.click square $ \_ -> onClick i gridRef stateRef
                on UI.contextmenu square $ \_ -> flagCell i gridRef stateRef


-- handles left click
onClick :: Int -> IORef Grid -> IORef GameState -> UI ()
onClick index gridRef stateRef = do
    gameState <- liftIO $ readIORef stateRef
    case gameState of
        -- when game not yet started
        -- updates game state and resets the grid
        -- then reveals cells
        GameStart numBombs -> do
            (Grid n _ _) <- liftIO $ readIORef gridRef
            liftIO $ writeIORef stateRef $ Playing (n*n - numBombs, numBombs)
            seed <- liftIO sysTime
            liftIO $ modifyIORef gridRef $ resetGrid numBombs index seed
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
flagCell :: Int -> IORef Grid -> IORef GameState -> UI ()
flagCell index gridRef stateRef = do
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
handleRestart :: IORef Grid -> IORef GameState -> Int -> UI ()
handleRestart gridRef stateRef numBombs = do
    Grid _ top cells <- liftIO $ readIORef gridRef
    resetTopBar top  -- reset title and flag count
    liftIO $ writeIORef stateRef (GameStart numBombs)  -- update game state
    V.mapM_ resetSquare cells  -- reset UI
    liftIO $ modifyIORef gridRef $ \grid -> grid { cells = V.map resetCell cells }  -- reset internal cells
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
fillBombs state = V.mapM_ (fillBomb GameOver)
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
