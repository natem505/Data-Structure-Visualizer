module Common.UI
    ( Button(..)
    , drawButton
    , insideButton
    , drawTitle
    , drawText
    ) where

import Graphics.Gloss

-- Button type for reusability
data Button = Button
    { buttonBounds :: (Float, Float, Float, Float)  -- (x1, y1, x2, y2)
    , buttonLabel :: String
    , buttonSelected :: Bool
    }

-- Check if point is inside button bounds
insideButton :: (Float, Float, Float, Float) -> Float -> Float -> Bool
insideButton (x1, y1, x2, y2) px py =
    px >= x1 && px <= x2 && py >= y1 && py <= y2

-- Draw a button (highlighted if selected)
drawButton :: Button -> Picture
drawButton (Button (x1, y1, x2, y2) label isSelected) =
    let centerX = (x1 + x2) / 2
        centerY = (y1 + y2) / 2
        width = x2 - x1
        height = y2 - y1
        bgColor = if isSelected
                  then makeColorI 70 130 180 255  -- blue if selected
                  else makeColorI 200 200 200 255 -- gray if not
        textColor = if isSelected then white else black
    in Pictures
        [ -- Button background
          color bgColor $ translate centerX centerY $ rectangleSolid width height
          -- Button border
        , color black $ translate centerX centerY $ rectangleWire width height
          -- Button label
        , translate (x1 + 10) (centerY - 5) $ scale 0.12 0.12 $ color textColor $ text label
        ]

-- Draw a title
drawTitle :: Float -> Float -> String -> Picture
drawTitle x y title =
    translate x y $ scale 0.3 0.3 $ color black $ text title

-- Draw regular text
drawText :: Float -> Float -> Float -> String -> Picture
drawText x y size txt =
    translate x y $ scale size size $ color black $ text txt