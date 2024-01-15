module Components where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Data.List.Split (chunksOf)


makeTitle :: UI Element
makeTitle = UI.div
        # set UI.text "Minesweeper"
        # set UI.style [
            ("width", "100%"),
            ("height", "50px"),
            ("line-height", "50px"),
            ("text-align", "center"),
            ("font-size", "20px"),
            ("font-family", "sans-serif"),
            ("font-weight", "bold"),
            ("user-select", "none")
        ]


displayGrid :: [Element] -> Int -> UI Element
displayGrid squares n = UI.div #+ [
        UI.grid (chunksOf n $ map element squares)
            # set UI.style [("margin", "auto"), ("border", "1px solid black")]
    ]

makeRestartButton :: UI Element
makeRestartButton = UI.div #+ [
        UI.button # set UI.text "↺"
            # set UI.style [
                ("margin", "auto"), 
                ("width", "40px"),
                ("height", "40px"),
                ("line-height", "40px"),
                ("background-color", "lightgrey"),
                ("border", "1px solid black"),
                ("text-align", "center"),
                ("font-size", "22px"),
                ("font-family", "sans-serif"),
                ("font-weight", "bold"),
                ("display", "inline-block"),
                ("vertical-align", "top"),
                ("user-select", "none")
            ]
    ]

makeBombCounter :: Int -> UI Element
makeBombCounter numBombs = UI.div 
    # set UI.text (show numBombs)
    # set UI.style [
        ("margin", "auto"), 
        ("width", "40px"),
        ("height", "40px"),
        ("line-height", "40px"),
        ("background-color", "lightgrey"),
        ("border", "1px solid black"),
        ("text-align", "center"),
        ("font-size", "22px"),
        ("font-family", "sans-serif"),
        ("font-weight", "bold"),
        ("display", "inline-block"),
        ("vertical-align", "top"),
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