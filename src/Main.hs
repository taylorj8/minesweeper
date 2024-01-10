module Main where

import Minesweeper

main :: IO ()
main = do
    print "Program started"
    print $ initGrid 10 25
    print "Program ended"