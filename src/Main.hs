module Main where

import Minesweeper

main :: IO ()
main = do
    print "Program started"
    print $ initGrid 5 10
    print "Program ended"