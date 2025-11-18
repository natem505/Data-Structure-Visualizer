module Render
    ( renderBST
    , renderBSTWithHighlight
    ) where

import Graphics.Gloss
import BST

-- Node with position information and subtree bounds
data Positioned a = Positioned
    { posValue :: a
    , posX :: Float
    , posY :: Float
    , posLeft :: Maybe (Positioned a)
    , posRight :: Maybe (Positioned a)
    , posLeftBound :: Float   -- leftmost x coordinate in this subtree
    , posRightBound :: Float  -- rightmost x coordinate in this subtree
    }

renderBST :: (Show a, Eq a) => BST a -> Picture
renderBST tree =
    let positioned = layoutTree tree
        defaultColor = makeColorI 220 50 50 255  -- red
    in drawPositioned positioned Nothing defaultColor

-- Render with a specific node highlighted with custom color
renderBSTWithHighlight :: (Show a, Eq a) => BST a -> Maybe a -> Color -> Picture
renderBSTWithHighlight tree highlight highlightColor =
    let positioned = layoutTree tree
    in drawPositioned positioned highlight highlightColor

-- Layout the tree with proper positioning
layoutTree :: BST a -> Maybe (Positioned a)
layoutTree Empty = Nothing
layoutTree tree = Just (positionTree tree 0 0)

-- Minimum horizontal spacing between adjacent nodes
minNodeSpacing :: Float
minNodeSpacing = 60

-- Position a tree centered at x, y
positionTree :: BST a -> Float -> Float -> Positioned a
positionTree Empty _ _ = error "Cannot position Empty tree"
positionTree (Node value left right) x y =
    let
        -- Recursively position left and right subtrees at origin first
        leftTree = case left of
            Empty -> Nothing
            l -> Just (positionTree l 0 (y - 90))

        rightTree = case right of
            Empty -> Nothing
            r -> Just (positionTree r 0 (y - 90))

        -- Calculate the width of left and right subtrees
        leftWidth = case leftTree of
            Nothing -> 0
            Just lt -> posRightBound lt - posLeftBound lt

        rightWidth = case rightTree of
            Nothing -> 0
            Just rt -> posRightBound rt - posLeftBound rt

        -- Calculate how far left/right to shift the subtrees
        -- Left subtree: its right edge should be minNodeSpacing/2 to the left of root
        leftShift = case leftTree of
            Nothing -> 0
            Just lt -> -(posRightBound lt + minNodeSpacing / 2)

        -- Right subtree: its left edge should be minNodeSpacing/2 to the right of root
        rightShift = case rightTree of
            Nothing -> 0
            Just rt -> minNodeSpacing / 2 - posLeftBound rt

        -- Shift the subtrees
        leftTreeShifted = case leftTree of
            Nothing -> Nothing
            Just lt -> Just (shiftTree lt leftShift 0)

        rightTreeShifted = case rightTree of
            Nothing -> Nothing
            Just rt -> Just (shiftTree rt rightShift 0)

        -- Calculate bounds of this tree
        leftBound = case leftTreeShifted of
            Nothing -> x - minNodeSpacing / 2
            Just lt -> min (x - minNodeSpacing / 2) (posLeftBound lt)

        rightBound = case rightTreeShifted of
            Nothing -> x + minNodeSpacing / 2
            Just rt -> max (x + minNodeSpacing / 2) (posRightBound rt)

    in Positioned value x y leftTreeShifted rightTreeShifted leftBound rightBound

-- Shift a positioned tree by dx, dy
shiftTree :: Positioned a -> Float -> Float -> Positioned a
shiftTree (Positioned v x y l r lb rb) dx dy =
    Positioned v (x + dx) (y + dy)
        (fmap (\t -> shiftTree t dx dy) l)
        (fmap (\t -> shiftTree t dx dy) r)
        (lb + dx)
        (rb + dx)

-- Draw a positioned tree with custom highlight color
drawPositioned :: (Show a, Eq a) => Maybe (Positioned a) -> Maybe a -> Color -> Picture
drawPositioned Nothing _ _ = Blank
drawPositioned (Just pos) highlight highlightColor =
    let isHighlighted = Just (posValue pos) == highlight
        -- Draw children first
        leftPic = case posLeft pos of
            Nothing -> Blank
            Just child -> Pictures
                [ drawEdge (posX pos) (posY pos) (posX child) (posY child)
                , drawPositioned (Just child) highlight highlightColor
                ]
        rightPic = case posRight pos of
            Nothing -> Blank
            Just child -> Pictures
                [ drawEdge (posX pos) (posY pos) (posX child) (posY child)
                , drawPositioned (Just child) highlight highlightColor
                ]
    in Pictures
        [ leftPic
        , rightPic
        , drawNode (posX pos) (posY pos) (posValue pos) isHighlighted highlightColor
        ]

-- Draw an edge between two nodes
drawEdge :: Float -> Float -> Float -> Float -> Picture
drawEdge x1 y1 x2 y2 =
    let angle = atan2 (y2 - y1) (x2 - x1)
        startX = x1 + 25 * cos angle
        startY = y1 + 25 * sin angle
        endX = x2 - 25 * cos angle
        endY = y2 - 25 * sin angle
    in color black $ line [(startX, startY), (endX, endY)]

-- Draw node with custom highlight color
drawNode :: Show a => Float -> Float -> a -> Bool -> Color -> Picture
drawNode x y v highlighted highlightColor =
    let nodeColor = if highlighted
                    then highlightColor  -- use custom color for highlighted
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