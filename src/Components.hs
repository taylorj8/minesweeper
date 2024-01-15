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
        ("background-color", "lightgrey"),
        ("border", "2px solid black"),
        ("text-align", "center"),
        ("font-size", "22px"),
        ("font-family", "sans-serif"),
        ("font-weight", "bold"),
        ("display", "inline-block"),
        ("vertical-align", "center"),
        ("user-select", "none")
    ]

uiCell :: UI Element
uiCell = UI.div # set UI.style [
        ("width", "25px"),
        ("height", "25px"),
        ("line-height", "25px"),
        ("background-color", "lightgrey"),
        ("border", "1px solid black"),
        ("text-align", "center"),
        ("font-size", "16px"),
        ("font-family", "sans-serif"),
        ("font-weight", "bold"),
        ("display", "inline-block"),
        ("vertical-align", "top"),
        ("user-select", "none")
    ]