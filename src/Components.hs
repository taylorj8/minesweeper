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
            ("font-size", "22px"),
            ("font-family", "sans-serif"),
            ("font-weight", "bold"),
            ("user-select", "none")
        ]

displayGrid :: [Element] -> Int -> UI Element
displayGrid squares n = UI.div #+ [
        UI.grid (chunksOf n $ map element squares)
            # set UI.style [("margin", "auto"), ("border", "1px solid black")]
    ]

topCell :: String -> UI Element
topCell text = UI.div 
    # set UI.text text
    # set UI.style [
        ("width", "40px"),
        ("height", "40px"),
        ("line-height", "40px"),
        ("border", "2px solid black"),
        ("font-size", "22px"),
        ("vertical-align", "center")
    ] # setCommonAttributes

uiCell :: UI Element
uiCell = UI.div # set UI.style [
        ("width", "25px"),
        ("height", "25px"),
        ("line-height", "25px"),
        ("border", "1px solid black"),
        ("font-size", "16px"),
        ("font-weight", "bold"),
        ("vertical-align", "top"),
        ("display", "inline-block")
    ] # setCommonAttributes

makeSolveButton :: UI Element
makeSolveButton = UI.div 
    # set UI.text "Play Move"
    # set UI.style [
        ("width", "100px"),
        ("height", "32px"),
        ("line-height", "32px"),
        ("border", "2px solid black"),
        ("font-size", "18px"),
        ("vertical-align", "top"),
        ("margin", "auto"),
        ("margin-top", "8px")
    ] # setCommonAttributes

-- attributes common to multiple components
setCommonAttributes :: UI Element -> UI Element
setCommonAttributes = set UI.style [
        ("text-align", "center"),
        ("font-family", "sans-serif"),
        ("user-select", "none"),
        ("background-color", "lightgrey"),
        ("text-align", "center"),
        ("font-family", "sans-serif")
    ]
    