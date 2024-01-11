module Threepenny where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Data.Maybe (isNothing)
import Text.Read (readMaybe)
import Control.Monad (when)
import Data.List (intercalate, isPrefixOf)
import Data.IORef ( IORef, newIORef, readIORef, writeIORef , modifyIORef)
import Control.Monad.Trans (liftIO)
import Data.Char (isDigit)


data Mode = Enter | Answer deriving Eq

numColor = "#6CCCE1"
opColor = "#159EBB"

main :: IO ()
main = do
  startGUI defaultConfig setup


setup window = do
  return window # set title "MineSweeper"

  screen <- UI.canvas
    # set UI.width 50
    # set UI.height 50
    # set UI.textFont "20px sans-serif"
    # set UI.style [
      ("border", "solid black 2px"),
      ("background", "#eee")
    ]

  -- IORef to store the current equation
  equation <- liftIO $ newIORef ""
  -- controls whether the user can enter new numbers
  mode <- liftIO $ newIORef Enter

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

-- style applied to all buttons
styledButton :: String -> String -> String -> UI Element
styledButton label color width = 
  UI.button #+ [string label]
    # set UI.style [
      ("border", "solid black 2px"), 
      ("background", color), 
      ("font-size", "30px"), 
      ("border-radius", "15px"),
      ("width", width),
      ("height", "60px"),
      ("margin", "4px")
      ]

-- digit is entered on screen when button pressed
enter :: IORef Mode -> UI.Canvas -> IORef String -> (String -> String) -> p -> UI ()
enter mode screen equation mod _ = do
  mode <- liftIO $ readIORef mode
  if mode == Answer then return () -- if in answer mode do nothing
  else do
    eq <- liftIO $ readIORef equation
    let eq' = mod eq
    liftIO $ writeIORef equation eq'
    screen # clearCanvas 
    screen # UI.fillText eq' (10, 32)

-- evaluates the current equation and sets mode to Answer
evaluateEquation :: IORef Mode -> UI.Canvas -> IORef String -> p -> UI ()
evaluateEquation modeRef screen equation _ = do
  eq <- liftIO $ readIORef equation
  enter modeRef screen equation (evaluate eq) ""
  liftIO $ writeIORef modeRef Answer

-- evaluates the expression by splitting it into a list of strings
-- then applying stepEval with each operator in order of precedence
evaluate :: String -> String -> String
evaluate str = do
  let eq = splitKeep "*/+_" $ handleNeg str
  let ans = head $ stepEval "_" $ stepEval "+" $ stepEval "/" $ stepEval "*" eq
  let ans' = if "--" `isPrefixOf` ans then "Parse Error" else ans -- final error check
  (++ "=" ++ ans')

-- splits a string into a list of strings, keeping the delimiters
-- multiple delimiters can be specified
-- "23+11*5" will be resolved to ["23", "+", "11", "*", "5"]
splitKeep :: [Char] -> String -> [String]
splitKeep _ [] = []
splitKeep delims str = 
  let (pre, post) = break (`elem` delims) str
  in pre : case post of
    [] -> []
    x:xs -> [x] : splitKeep delims xs

-- steps through equation, evaluating one operator at a time
-- by passing in operators in order of precedence, it will correctly evaluate an equation
stepEval :: String -> [String] -> [String]
stepEval _ [] = []
stepEval _ [x] = [x]
stepEval op (x:op':y:xs) = if op == op' then stepEval op $ applyOp x op y : xs else x : stepEval op (op':y:xs)
stepEval op (x:xs) = x : stepEval op xs

-- applies an operator to two numbers represented as strings
applyOp :: String -> String -> String -> String
applyOp x op y = case (x', y') of
  (Just x', Just y') -> show $ applyOp' x' op y'
  _ -> "Parse Error"
  where 
    x' = readMaybe x :: Maybe Double
    y' = readMaybe y :: Maybe Double

applyOp' :: Double -> String -> Double -> Double
applyOp' x op y 
  | op == "+" = x + y
  | op == "_" = x - y
  | op == "*" = x * y
  | op == "/" = x / y
  | otherwise = error "Parse Error"

-- replaces - with _ if it comes after a digit
-- allows for negation as well as subtraction
handleNeg :: String -> String
handleNeg "" = ""
handleNeg [x] = [x]
handleNeg (x:y:xs)
  | y == '-' && isDigit x = x : '_' : handleNeg xs
  | otherwise = x : handleNeg (y:xs)
