module Components where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Data.List.Split (chunksOf)

-- file contains various UI components

makeTitle :: UI Element
makeTitle = UI.div
        # set UI.text "Minesweeper"
        # set UI.style [
            ("width", "250px"),
            ("height", "40px"),
            ("text-align", "center"),
            ("font-size", "23px"),
            ("font-family", "Trebuchet MS"),
            ("font-weight", "bold"),
            ("user-select", "none"),
            ("color", "whitesmoke")
        ]

displayGrid :: [Element] -> Int -> UI Element
displayGrid squares n = UI.div #+ [
        UI.grid (chunksOf n $ map element squares)
            # set UI.style [("margin", "auto"), ("border", "1px solid black")]
    ]

uiCell :: UI Element
uiCell = UI.div # set UI.style [
        ("width", "25px"),
        ("height", "25px"),
        ("line-height", "25px"),
        ("border", "1px solid black"),
        ("font-size", "16px"),
        ("font-weight", "bold"),
        ("vertical-align", "top"),
        ("display", "inline-block"),
        ("font-family", "Trebuchet MS")
    ] # setCommonAttributes

topCell :: String -> UI Element
topCell text = UI.div 
    # set UI.text text
    # set UI.style [
        ("width", "40px"),
        ("height", "40px"),
        ("line-height", "40px"),
        ("border", "1px solid black"),
        ("font-size", "22px"),
        ("vertical-align", "center"),
        ("font-family", "sans-serif"),
        ("border-radius", "5px")
    ] # setCommonAttributes

makeSolveButton :: String -> UI Element
makeSolveButton text = UI.div 
    # set UI.text text
    # set UI.style [
        ("width", "100px"),
        ("height", "32px"),
        ("line-height", "32px"),
        ("border", "1px solid black"),
        ("font-size", "18px"),
        ("vertical-align", "top"),
        ("margin-top", "8px"),
        ("border-radius", "5px")
    ] # setCommonAttributes

makeProbText :: UI Element
makeProbText = UI.div
        # set UI.style [
            ("width", "120px"),
            ("height", "32px"),
            ("text-align", "center"),
            ("font-size", "18px"),
            ("font-family", "Trebuchet MS"),
            ("user-select", "none"),
            ("color", "whitesmoke")
        ]

-- attributes common to multiple components
setCommonAttributes :: UI Element -> UI Element
setCommonAttributes = set UI.style [
        ("text-align", "center"),
        ("font-family", "sans-serif"),
        ("user-select", "none"),
        ("background-color", "lightgrey"),
        ("text-align", "center")
    ]

-- style the background
setBackgroundStyle :: String -> UI Element -> UI Element
setBackgroundStyle image = set UI.style [
        ("background-image", "url('/static/" ++ image ++ "')"),
        ("background-size", "cover"),
        ("background-repeat", "no-repeat"),
        ("background-position", "center center"),
        ("background-attachment", "fixed")
    ]