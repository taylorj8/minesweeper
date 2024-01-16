module Solver where

import Util

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)

solve :: IORef Grid -> IORef GameState -> UI ()
solve _ _ = return ()