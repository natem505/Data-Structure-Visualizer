module Render
    ( renderBST
    , renderBSTWithHighlight
    ) where

import Graphics.Gloss
import BST

-- Added Eq constraint here
renderBST :: (Show a, Eq a) => BST a -> Picture
renderBST tree = translate 0 200 $ drawTree tree 0 0 180 Nothing

-- Render with a specific node highlighted
renderBSTWithHighlight :: (Show a, Eq a) => BST a -> Maybe a -> Float -> Float -> Float -> Picture
renderBSTWithHighlight tree highlight x y dx =
    drawTree tree x y dx highlight

-- Now takes a "highlight" parameter to know which node to color red
drawTree :: (Show a, Eq a) => BST a -> Float -> Float -> Float -> Maybe a -> Picture
drawTree Empty _ _ _ _ = Blank

drawTree (Node value left right) x y dx highlight =
    let isHighlighted = Just value == highlight
    in Pictures
        [ drawLeft  left  x y dx highlight
        , drawRight right x y dx highlight
        , drawNode x y value isHighlighted
        ]

-- Draw node: RED if highlighted, BLUE if normal
drawNode :: Show a => Float -> Float -> a -> Bool -> Picture
drawNode x y v highlighted =
    let nodeColor = if highlighted
                    then makeColorI 220 50 50 255  -- red for current node
                    else makeColorI 70 130 180 255  -- blue for normal
    in Pictures
        [ color nodeColor $ translate x y $ circleSolid 25
        , color black $ translate x y $ circle 25
        , centerText x y (show v)
        ]

-- Bold text rendering with dynamic sizing based on string length
centerText :: Float -> Float -> String -> Picture
centerText x y s =
    let -- Scale down for longer numbers
        baseScale = 0.18
        scaleFactor = if length s <= 2
                      then baseScale
                      else if length s == 3
                           then baseScale * 0.75  -- 3 digits: 75% size
                           else baseScale * 0.6   -- 4+ digits: 60% size
        (w, h)     = textSize s
        xOffset = -10
        yOffset = 0
        baseText = scale scaleFactor scaleFactor
                   $ translate (-w/2 + xOffset) (-h/2 + yOffset)
                   $ text s
        boldText = Pictures
            [ baseText
            , translate 0.5 0 baseText
            , translate 0 0.5 baseText
            , translate 0.5 0.5 baseText
            ]
    in translate x y $ color black $ boldText

textSize :: String -> (Float, Float)
textSize s = (fromIntegral (length s) * 50, 70)

-- Draw left subtree with edges - increased minimum spacing
drawLeft :: (Show a, Eq a) => BST a -> Float -> Float -> Float -> Maybe a -> Picture
drawLeft Empty _ _ _ _ = Blank
drawLeft subtree x y dx highlight =
    let childX = x - dx
        childY = y - 90
        angle = atan2 (childY - y) (childX - x)
        startX = x + 25 * cos angle
        startY = y + 25 * sin angle
        endX = childX - 25 * cos angle
        endY = childY - 25 * sin angle
        -- Less aggressive reduction
        newDx = dx * 0.7
        -- Increased minimum spacing to 55 pixels
        minSpacing = 55
        finalDx = max newDx minSpacing
    in Pictures
        [ color black $ line [(startX, startY), (endX, endY)]
        , drawTree subtree childX childY finalDx highlight
        ]

-- Draw right subtree with edges - increased minimum spacing
drawRight :: (Show a, Eq a) => BST a -> Float -> Float -> Float -> Maybe a -> Picture
drawRight Empty _ _ _ _ = Blank
drawRight subtree x y dx highlight =
    let childX = x + dx
        childY = y - 90
        angle = atan2 (childY - y) (childX - x)
        startX = x + 25 * cos angle
        startY = y + 25 * sin angle
        endX = childX - 25 * cos angle
        endY = childY - 25 * sin angle
        -- Less aggressive reduction
        newDx = dx * 0.7
        -- Increased minimum spacing to 55 pixels
        minSpacing = 55
        finalDx = max newDx minSpacing
    in Pictures
        [ color black $ line [(startX, startY), (endX, endY)]
        , drawTree subtree childX childY finalDx highlight
        ]