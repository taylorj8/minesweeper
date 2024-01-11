module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Data.IORef ( IORef, newIORef, readIORef, writeIORef , modifyIORef)

import Minesweeper

-- data State = Menu | Playing | GameOver deriving Eq

main :: IO ()
main = do
    startGUI defaultConfig setup


setup window = do
    return window # set title "MineSweeper"


    -- IORef to store the current equation
    equation <- liftIO $ newIORef ""
--   state <- liftIO $ newIORef Menu

    b0 <- styledButton "0" numColor "60px"
    b1 <- styledButton "1" numColor "60px"
    b2 <- styledButton "2" numColor "60px"
    b3 <- styledButton "3" numColor "60px"
    b4 <- styledButton "4" numColor "60px"
    b5 <- styledButton "5" numColor "60px"
    b6 <- styledButton "6" numColor "60px"
    b7 <- styledButton "7" numColor "60px"
    b8 <- styledButton "8" numColor "60px"
    b9 <- styledButton "9" numColor "60px"
    bDot <- styledButton "." numColor "60px"
    bPlus <- styledButton "+" opColor "60px"
    bMinus <- styledButton "-" opColor "60px"
    bTimes <- styledButton "*" opColor "60px"
    bDivide <- styledButton "/" opColor "60px"
    bEquals <- styledButton "=" opColor "60px"
    bAC <- styledButton "AC" opColor "128px"
    bDel <- styledButton "Del" opColor "128px"

    getBody window #+
      [
        element screen,
        row [element bAC, element bDel],
        row [element b7, element b8, element b9, element bDivide], 
        row [element b4, element b5, element b6, element bTimes], 
        row [element b1, element b2, element b3, element bMinus], 
        row [element b0, element bDot, element bEquals, element bPlus]
      ] 

    on UI.click bAC $ const $ do
      liftIO $ writeIORef equation ""
      screen # clearCanvas
      liftIO $ writeIORef mode Enter
    on UI.click bDel $ enter mode screen equation init
    on UI.click b0 $ enter mode screen equation (++ "0")
    on UI.click b1 $ enter mode screen equation (++ "1")
    on UI.click b2 $ enter mode screen equation (++ "2")
    on UI.click b3 $ enter mode screen equation (++ "3")
    on UI.click b4 $ enter mode screen equation (++ "4")
    on UI.click b5 $ enter mode screen equation (++ "5")
    on UI.click b6 $ enter mode screen equation (++ "6")
    on UI.click b7 $ enter mode screen equation (++ "7")
    on UI.click b8 $ enter mode screen equation (++ "8")
    on UI.click b9 $ enter mode screen equation (++ "9")
    on UI.click bDot $ enter mode screen equation (++ ".")
    on UI.click bPlus $ enter mode screen equation (++ "+")
    on UI.click bMinus $ enter mode screen equation (++ "-")
    on UI.click bTimes $ enter mode screen equation (++ "*")
    on UI.click bDivide $ enter mode screen equation (++ "/")
    on UI.click bEquals $ evaluateEquation mode screen equation


cell :: Cell -> UI Element
