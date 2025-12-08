{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Common.UI
import qualified DataStructures.BST.BST as BST
import qualified DataStructures.AVL.AVL as AVL
import qualified DataStructures.Heap.Heap as Heap
import qualified DataStructures.HashTable.HashTable as HT
import Control.Exception (catch, SomeException)

-- Main application state - which screen are we on?
data AppState = MenuScreen | BSTScreen BSTWorld | AVLScreen AVLWorld | HeapScreen HeapWorld | HashTableScreen HashTableWorld

-- ============= BST TYPES =============

data BSTWorld = BSTWorld
    { bstTree :: BST.BST Int
    , bstSteps :: [BST.Step Int]
    , bstCurrentStep :: Int
    , bstValue :: String
    , bstMode :: BSTMode
    , bstOperation :: BSTOperation
    , bstSearchResult :: Maybe Bool
    , bstPendingTree :: Maybe (BST.BST Int)
    , bstPaused :: Bool
    }

data BSTMode = BSTIdle | BSTAnimating
    deriving (Eq)

data BSTOperation = BSTInsert | BSTSearch | BSTDelete | BSTInOrder | BSTPreOrder | BSTPostOrder
    deriving (Eq, Show)

data Positioned a = Positioned
    { posValue :: a
    , posX :: Float
    , posY :: Float
    , posLeft :: Maybe (Positioned a)
    , posRight :: Maybe (Positioned a)
    , posLeftBound :: Float
    , posRightBound :: Float
    }

-- ============= AVL TYPES =============

data AVLWorld = AVLWorld
    { avlTree :: AVL.AVL Int
    , avlSteps :: [AVL.Step Int]
    , avlCurrentStep :: Int
    , avlValue :: String
    , avlMode :: AVLMode
    , avlOperation :: AVLOperation
    , avlSearchResult :: Maybe Bool
    , avlPendingTree :: Maybe (AVL.AVL Int)
    , avlPaused :: Bool
    }

data AVLMode = AVLIdle | AVLAnimating
    deriving (Eq)

data AVLOperation = AVLInsert | AVLSearch | AVLDelete
    deriving (Eq, Show)

data PositionedAVL a = PositionedAVL
    { avlPosValue :: a
    , avlPosHeight :: Int
    , avlPosX :: Float
    , avlPosY :: Float
    , avlPosLeft :: Maybe (PositionedAVL a)
    , avlPosRight :: Maybe (PositionedAVL a)
    , avlPosLeftBound :: Float
    , avlPosRightBound :: Float
    }

-- ============= HEAP TYPES =============

data HeapWorld = HeapWorld
    { heapData :: Heap.Heap Int
    , heapSteps :: [Heap.Step Int]
    , heapCurrentStep :: Int
    , heapValue :: String
    , heapMode :: HeapMode
    , heapOperation :: HeapOperation
    , heapPendingHeap :: Maybe (Heap.Heap Int)
    , heapExtractedValue :: Maybe Int
    , heapLastSorted :: Maybe [Int]
    , heapPaused :: Bool
    }

data HeapMode = HeapIdle | HeapAnimating
    deriving (Eq)

data HeapOperation = HeapInsert | HeapExtract | HeapSort
    deriving (Eq, Show)

-- ============= HASH TABLE TYPES =============

data HashTableWorld = HashTableWorld
    { htTable :: HT.HashTable Int
    , htSteps :: [HT.Step Int]
    , htCurrentStep :: Int
    , htValue :: String
    , htMode :: HTMode
    , htOperation :: HTOperation
    , htSearchResult :: Maybe Bool
    , htPendingTable :: Maybe (HT.HashTable Int)
    , htPaused :: Bool
    }

data HTMode = HTIdle | HTAnimating
    deriving (Eq)

data HTOperation = HTInsert | HTSearch | HTDelete
    deriving (Eq, Show)

-- ============= MAIN =============

main :: IO ()
main = catch
    (playIO
        (InWindow "Data Structure Visualizer" (1200, 800) (100, 100))
        white
        1
        MenuScreen
        (return . drawApp)
        handleAppEvent
        updateApp)
    (\(_ :: SomeException) -> putStrLn "Goodbye!")

-- ============= MENU SCREEN =============

menuBSTBounds :: (Float, Float, Float, Float)
menuBSTBounds = (-150, 125, 150, 175)

menuAVLBounds :: (Float, Float, Float, Float)
menuAVLBounds = (-150, 55, 150, 105)

menuHeapBounds :: (Float, Float, Float, Float)
menuHeapBounds = (-150, -15, 150, 35)

menuHashTableBounds :: (Float, Float, Float, Float)
menuHashTableBounds = (-150, -85, 150, -35)

menuExitBounds :: (Float, Float, Float, Float)
menuExitBounds = (-150, -155, 150, -105)

drawMenu :: Picture
drawMenu = Pictures
    [ drawTitle (-300) 250 "Data Structure Visualizer"
    , drawText (-300) 200 0.12 "Select a data structure:"
    , drawButton $ Button menuBSTBounds "Binary Search Tree" False
    , drawButton $ Button menuAVLBounds "AVL Tree" False
    , drawButton $ Button menuHeapBounds "Max Heap" False
    , drawButton $ Button menuHashTableBounds "Hash Table" False
    , drawButton $ Button menuExitBounds "Exit" False
    ]

-- ============= BST SCREEN =============

initialBSTWorld :: BSTWorld
initialBSTWorld = BSTWorld
    { bstTree = BST.Empty
    , bstSteps = []
    , bstCurrentStep = 0
    , bstValue = ""
    , bstMode = BSTIdle
    , bstOperation = BSTInsert
    , bstSearchResult = Nothing
    , bstPendingTree = Nothing
    , bstPaused = False
    }

-- ============= BUTTON BOUNDS - REORGANIZED =============

-- Navigation
backButtonBounds :: (Float, Float, Float, Float)
backButtonBounds = (-580, 360, -480, 390)

-- BST/AVL Operations (top row)
insertButtonBounds :: (Float, Float, Float, Float)
insertButtonBounds = (-580, 310, -480, 345)

searchButtonBounds :: (Float, Float, Float, Float)
searchButtonBounds = (-470, 310, -370, 345)

deleteButtonBounds :: (Float, Float, Float, Float)
deleteButtonBounds = (-360, 310, -260, 345)

-- Traversals (second row for BST only)
inOrderButtonBounds :: (Float, Float, Float, Float)
inOrderButtonBounds = (-580, 265, -480, 300)

preOrderButtonBounds :: (Float, Float, Float, Float)
preOrderButtonBounds = (-470, 265, -370, 300)

postOrderButtonBounds :: (Float, Float, Float, Float)
postOrderButtonBounds = (-360, 265, -260, 300)

-- Heap operations
extractButtonBounds :: (Float, Float, Float, Float)
extractButtonBounds = (-470, 310, -370, 345)

heapSortButtonBounds :: (Float, Float, Float, Float)
heapSortButtonBounds = (-360, 310, -260, 345)

-- Replay controls (bottom row - always visible)
playPauseButtonBounds :: (Float, Float, Float, Float)
playPauseButtonBounds = (-580, 215, -500, 250)

prevStepButtonBounds :: (Float, Float, Float, Float)
prevStepButtonBounds = (-490, 215, -420, 250)

nextStepButtonBounds :: (Float, Float, Float, Float)
nextStepButtonBounds = (-410, 215, -340, 250)

restartButtonBounds :: (Float, Float, Float, Float)
restartButtonBounds = (-330, 215, -260, 250)

drawBSTWorld :: BSTWorld -> Picture
drawBSTWorld world =
    let currentTree = if bstMode world == BSTAnimating && bstCurrentStep world < length (bstSteps world)
                      then BST.stepTree (bstSteps world !! bstCurrentStep world)
                      else bstTree world
    in Pictures
        [ translate 0 150 $ renderBSTWithHighlight
            currentTree
            (getCurrentBSTHighlight world)
            (getBSTHighlightColor world)
        , drawBSTButtons world
        , if bstMode world == BSTIdle
          then drawText (-580) 180 0.12 ("Input: " ++ bstValue world)
          else drawText (-580) 180 0.12 ("Step " ++ show (bstCurrentStep world + 1) ++ " / " ++ show (length (bstSteps world)))
        , drawBSTStatus world
        ]

renderBSTWithHighlight :: (Show a, Eq a) => BST.BST a -> Maybe a -> Color -> Picture
renderBSTWithHighlight tree highlight highlightColor =
    let positioned = layoutTree tree
    in drawPositioned positioned highlight highlightColor

layoutTree :: BST.BST a -> Maybe (Positioned a)
layoutTree BST.Empty = Nothing
layoutTree tree = Just (positionTree tree 0 0)

minNodeSpacing :: Float
minNodeSpacing = 60

positionTree :: BST.BST a -> Float -> Float -> Positioned a
positionTree BST.Empty _ _ = error "Cannot position Empty tree"
positionTree (BST.Node value left right) x y =
    let
        leftTree = case left of
            BST.Empty -> Nothing
            l -> Just (positionTree l 0 (y - 90))

        rightTree = case right of
            BST.Empty -> Nothing
            r -> Just (positionTree r 0 (y - 90))

        leftShift = case leftTree of
            Nothing -> 0
            Just lt -> -(posRightBound lt + minNodeSpacing / 2)

        rightShift = case rightTree of
            Nothing -> 0
            Just rt -> minNodeSpacing / 2 - posLeftBound rt

        leftTreeShifted = case leftTree of
            Nothing -> Nothing
            Just lt -> Just (shiftTree lt leftShift 0)

        rightTreeShifted = case rightTree of
            Nothing -> Nothing
            Just rt -> Just (shiftTree rt rightShift 0)

        leftBound = case leftTreeShifted of
            Nothing -> x - minNodeSpacing / 2
            Just lt -> min (x - minNodeSpacing / 2) (posLeftBound lt)

        rightBound = case rightTreeShifted of
            Nothing -> x + minNodeSpacing / 2
            Just rt -> max (x + minNodeSpacing / 2) (posRightBound rt)

    in Positioned value x y leftTreeShifted rightTreeShifted leftBound rightBound

shiftTree :: Positioned a -> Float -> Float -> Positioned a
shiftTree (Positioned v x y l r lb rb) dx dy =
    Positioned v (x + dx) (y + dy)
        (fmap (\t -> shiftTree t dx dy) l)
        (fmap (\t -> shiftTree t dx dy) r)
        (lb + dx)
        (rb + dx)

drawPositioned :: (Show a, Eq a) => Maybe (Positioned a) -> Maybe a -> Color -> Picture
drawPositioned Nothing _ _ = Blank
drawPositioned (Just pos) highlight highlightColor =
    let isHighlighted = Just (posValue pos) == highlight
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

drawEdge :: Float -> Float -> Float -> Float -> Picture
drawEdge x1 y1 x2 y2 =
    let angle = atan2 (y2 - y1) (x2 - x1)
        startX = x1 + 25 * cos angle
        startY = y1 + 25 * sin angle
        endX = x2 - 25 * cos angle
        endY = y2 - 25 * sin angle
    in color black $ line [(startX, startY), (endX, endY)]

drawNode :: Show a => Float -> Float -> a -> Bool -> Color -> Picture
drawNode x y v highlighted highlightColor =
    let nodeColor = if highlighted
                    then highlightColor
                    else makeColorI 70 130 180 255
    in Pictures
        [ color nodeColor $ translate x y $ circleSolid 25
        , color black $ translate x y $ circle 25
        , centerText x y (show v)
        ]

centerText :: Float -> Float -> String -> Picture
centerText x y s =
    let baseScale = 0.18
        scaleFactor = if length s <= 2
                      then baseScale
                      else if length s == 3
                           then baseScale * 0.75
                           else baseScale * 0.6
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

getBSTHighlightColor :: BSTWorld -> Color
getBSTHighlightColor world = case bstOperation world of
    BSTInsert -> makeColorI 220 50 50 255
    BSTSearch -> makeColorI 255 165 0 255
    BSTDelete -> makeColorI 180 50 50 255
    BSTInOrder -> makeColorI 100 200 100 255
    BSTPreOrder -> makeColorI 100 200 100 255
    BSTPostOrder -> makeColorI 100 200 100 255

drawBSTButtons :: BSTWorld -> Picture
drawBSTButtons world = Pictures
    [ -- Navigation
      drawButton $ Button backButtonBounds "< Back" False

      -- Operations Row 1
    , drawButton $ Button insertButtonBounds "Insert" (bstOperation world == BSTInsert)
    , drawButton $ Button searchButtonBounds "Search" (bstOperation world == BSTSearch)
    , drawButton $ Button deleteButtonBounds "Delete" (bstOperation world == BSTDelete)

      -- Traversals Row 2
    , drawButton $ Button inOrderButtonBounds "In-Order" (bstOperation world == BSTInOrder)
    , drawButton $ Button preOrderButtonBounds "Pre-Order" (bstOperation world == BSTPreOrder)
    , drawButton $ Button postOrderButtonBounds "Post-Order" (bstOperation world == BSTPostOrder)

      -- Replay controls Row 3 - ALWAYS visible
    , drawButton $ Button playPauseButtonBounds (if bstPaused world then "▶ Play" else "⏸ Pause") (bstMode world == BSTAnimating)
    , drawButton $ Button prevStepButtonBounds "◀ Prev" (bstMode world == BSTAnimating)
    , drawButton $ Button nextStepButtonBounds "Next ▶" (bstMode world == BSTAnimating)
    , drawButton $ Button restartButtonBounds "⟲ Restart" (bstMode world == BSTAnimating)
    ]

drawBSTStatus :: BSTWorld -> Picture
drawBSTStatus world
    | bstMode world == BSTAnimating && bstCurrentStep world < length (bstSteps world) =
        let step = bstSteps world !! bstCurrentStep world
        in drawText (-550) (-350) 0.12 (BST.stepDesc step)
    | bstMode world == BSTIdle =
        let modeText = "Mode: " ++ show (bstOperation world)
            resultText = case bstSearchResult world of
                Just True -> " | Last search: FOUND"
                Just False -> " | Last search: NOT FOUND"
                Nothing -> ""
        in drawText (-550) (-350) 0.12 (modeText ++ resultText)
    | otherwise = Blank

getCurrentBSTHighlight :: BSTWorld -> Maybe Int
getCurrentBSTHighlight world
    | bstMode world == BSTAnimating && bstCurrentStep world < length (bstSteps world) =
        BST.stepHighlight (bstSteps world !! bstCurrentStep world)
    | otherwise = Nothing

-- ============= AVL SCREEN =============

initialAVLWorld :: AVLWorld
initialAVLWorld = AVLWorld
    { avlTree = AVL.Empty
    , avlSteps = []
    , avlCurrentStep = 0
    , avlValue = ""
    , avlMode = AVLIdle
    , avlOperation = AVLInsert
    , avlSearchResult = Nothing
    , avlPendingTree = Nothing
    , avlPaused = False
    }

drawAVLWorld :: AVLWorld -> Picture
drawAVLWorld world =
    let currentTree = if avlMode world == AVLAnimating && avlCurrentStep world < length (avlSteps world)
                      then AVL.stepTree (avlSteps world !! avlCurrentStep world)
                      else avlTree world
        currentStep = if avlMode world == AVLAnimating && avlCurrentStep world < length (avlSteps world)
                      then Just (avlSteps world !! avlCurrentStep world)
                      else Nothing
        rotationType = case currentStep of
                        Just step -> AVL.stepRotationType step
                        Nothing -> Nothing
        highlightColor = case rotationType of
                          Just _ -> makeColorI 255 140 0 255
                          Nothing -> getAVLHighlightColor world
    in Pictures
        [ translate 0 150 $ renderAVLWithHighlight
            currentTree
            (getCurrentAVLHighlight world)
            highlightColor
        , drawAVLButtons world
        , if avlMode world == AVLIdle
          then drawText (-580) 180 0.12 ("Input: " ++ avlValue world)
          else drawText (-580) 180 0.12 ("Step " ++ show (avlCurrentStep world + 1) ++ " / " ++ show (length (avlSteps world)))
        , drawText (-580) 150 0.1 "AVL Tree - Self-Balancing!"
        , drawAVLStatus world
        , case rotationType of
            Just rt -> translate (-150) (-280) $ scale 0.25 0.25 $ color red $ text ("ROTATION: " ++ show rt)
            Nothing -> Blank
        ]

renderAVLWithHighlight :: (Show a, Eq a) => AVL.AVL a -> Maybe a -> Color -> Picture
renderAVLWithHighlight tree highlight highlightColor =
    let positioned = layoutAVLTree tree
    in drawAVLPositioned positioned highlight highlightColor

layoutAVLTree :: AVL.AVL a -> Maybe (PositionedAVL a)
layoutAVLTree AVL.Empty = Nothing
layoutAVLTree tree = Just (positionAVLTree tree 0 0)

positionAVLTree :: AVL.AVL a -> Float -> Float -> PositionedAVL a
positionAVLTree AVL.Empty _ _ = error "Cannot position Empty tree"
positionAVLTree (AVL.Node value h left right) x y =
    let
        leftTree = case left of
            AVL.Empty -> Nothing
            l -> Just (positionAVLTree l 0 (y - 90))

        rightTree = case right of
            AVL.Empty -> Nothing
            r -> Just (positionAVLTree r 0 (y - 90))

        leftShift = case leftTree of
            Nothing -> 0
            Just lt -> -(avlPosRightBound lt + minNodeSpacing / 2)

        rightShift = case rightTree of
            Nothing -> 0
            Just rt -> minNodeSpacing / 2 - avlPosLeftBound rt

        leftTreeShifted = case leftTree of
            Nothing -> Nothing
            Just lt -> Just (shiftAVLTree lt leftShift 0)

        rightTreeShifted = case rightTree of
            Nothing -> Nothing
            Just rt -> Just (shiftAVLTree rt rightShift 0)

        leftBound = case leftTreeShifted of
            Nothing -> x - minNodeSpacing / 2
            Just lt -> min (x - minNodeSpacing / 2) (avlPosLeftBound lt)

        rightBound = case rightTreeShifted of
            Nothing -> x + minNodeSpacing / 2
            Just rt -> max (x + minNodeSpacing / 2) (avlPosRightBound rt)

    in PositionedAVL value h x y leftTreeShifted rightTreeShifted leftBound rightBound

shiftAVLTree :: PositionedAVL a -> Float -> Float -> PositionedAVL a
shiftAVLTree (PositionedAVL v h x y l r lb rb) dx dy =
    PositionedAVL v h (x + dx) (y + dy)
        (fmap (\t -> shiftAVLTree t dx dy) l)
        (fmap (\t -> shiftAVLTree t dx dy) r)
        (lb + dx)
        (rb + dx)

drawAVLPositioned :: (Show a, Eq a) => Maybe (PositionedAVL a) -> Maybe a -> Color -> Picture
drawAVLPositioned Nothing _ _ = Blank
drawAVLPositioned (Just pos) highlight highlightColor =
    let isHighlighted = Just (avlPosValue pos) == highlight
        bf = let lh = case avlPosLeft pos of
                    Nothing -> 0
                    Just l -> avlPosHeight l
                 rh = case avlPosRight pos of
                    Nothing -> 0
                    Just r -> avlPosHeight r
             in lh - rh
        leftPic = case avlPosLeft pos of
            Nothing -> Blank
            Just child -> Pictures
                [ drawEdge (avlPosX pos) (avlPosY pos) (avlPosX child) (avlPosY child)
                , drawAVLPositioned (Just child) highlight highlightColor
                ]
        rightPic = case avlPosRight pos of
            Nothing -> Blank
            Just child -> Pictures
                [ drawEdge (avlPosX pos) (avlPosY pos) (avlPosX child) (avlPosY child)
                , drawAVLPositioned (Just child) highlight highlightColor
                ]
    in Pictures
        [ leftPic
        , rightPic
        , drawAVLNode (avlPosX pos) (avlPosY pos) (avlPosValue pos) (avlPosHeight pos) bf isHighlighted highlightColor
        ]

drawAVLNode :: Show a => Float -> Float -> a -> Int -> Int -> Bool -> Color -> Picture
drawAVLNode x y v h bf highlighted highlightColor =
    let nodeColor = if highlighted
                    then highlightColor
                    else makeColorI 70 130 180 255
        bfColor = if abs bf > 1 then red else makeColorI 0 150 0 255
    in Pictures
        [ color nodeColor $ translate x y $ circleSolid 25
        , color black $ translate x y $ circle 25
        , centerText x y (show v)
        , translate (x + 30) (y + 10) $ scale 0.1 0.1 $ color (greyN 0.4) $ text ("h=" ++ show h)
        , translate (x + 30) (y - 10) $ scale 0.1 0.1 $ color bfColor $ text ("bf=" ++ show bf)
        ]

getAVLHighlightColor :: AVLWorld -> Color
getAVLHighlightColor world = case avlOperation world of
    AVLInsert -> makeColorI 220 50 50 255
    AVLSearch -> makeColorI 255 165 0 255
    AVLDelete -> makeColorI 180 50 50 255

drawAVLButtons :: AVLWorld -> Picture
drawAVLButtons world = Pictures
    [ -- Navigation
      drawButton $ Button backButtonBounds "< Back" False

      -- Operations Row 1
    , drawButton $ Button insertButtonBounds "Insert" (avlOperation world == AVLInsert)
    , drawButton $ Button searchButtonBounds "Search" (avlOperation world == AVLSearch)
    , drawButton $ Button deleteButtonBounds "Delete" (avlOperation world == AVLDelete)

      -- Replay controls Row 2 - ALWAYS visible
    , drawButton $ Button playPauseButtonBounds (if avlPaused world then "▶ Play" else "⏸ Pause") (avlMode world == AVLAnimating)
    , drawButton $ Button prevStepButtonBounds "◀ Prev" (avlMode world == AVLAnimating)
    , drawButton $ Button nextStepButtonBounds "Next ▶" (avlMode world == AVLAnimating)
    , drawButton $ Button restartButtonBounds "⟲ Restart" (avlMode world == AVLAnimating)
    ]

drawAVLStatus :: AVLWorld -> Picture
drawAVLStatus world
    | avlMode world == AVLAnimating && avlCurrentStep world < length (avlSteps world) =
        let step = avlSteps world !! avlCurrentStep world
        in drawText (-550) (-350) 0.12 (AVL.stepDesc step)
    | avlMode world == AVLIdle =
        let modeText = "Mode: " ++ show (avlOperation world)
            resultText = case avlSearchResult world of
                Just True -> " | Last search: FOUND"
                Just False -> " | Last search: NOT FOUND"
                Nothing -> ""
        in drawText (-550) (-350) 0.12 (modeText ++ resultText)
    | otherwise = Blank

getCurrentAVLHighlight :: AVLWorld -> Maybe Int
getCurrentAVLHighlight world
    | avlMode world == AVLAnimating && avlCurrentStep world < length (avlSteps world) =
        AVL.stepHighlight (avlSteps world !! avlCurrentStep world)
    | otherwise = Nothing

-- ============= HEAP SCREEN =============

initialHeapWorld :: HeapWorld
initialHeapWorld = HeapWorld
    { heapData = Heap.empty
    , heapSteps = []
    , heapCurrentStep = 0
    , heapValue = ""
    , heapMode = HeapIdle
    , heapOperation = HeapInsert
    , heapPendingHeap = Nothing
    , heapExtractedValue = Nothing
    , heapLastSorted = Nothing
    , heapPaused = False
    }

drawHeapWorld :: HeapWorld -> Picture
drawHeapWorld world =
    let currentHeap = if heapMode world == HeapAnimating && heapCurrentStep world < length (heapSteps world)
                      then Heap.stepHeap (heapSteps world !! heapCurrentStep world)
                      else heapData world
        currentStep = if heapMode world == HeapAnimating && heapCurrentStep world < length (heapSteps world)
                      then Just (heapSteps world !! heapCurrentStep world)
                      else Nothing
        highlightIdx = case currentStep of
                        Just step -> Heap.stepHighlight step
                        Nothing -> Nothing
        compareIdxs = case currentStep of
                       Just step -> Heap.stepCompareIndices step
                       Nothing -> Nothing
        sortedArray = case currentStep of
                       Just step -> Heap.stepSortedArray step
                       Nothing -> heapLastSorted world
    in Pictures
        [ translate 0 150 $ drawHeapAsTree currentHeap highlightIdx compareIdxs
        , translate 0 (-150) $ drawHeapAsArray currentHeap highlightIdx compareIdxs
        , case sortedArray of
            Just arr -> translate 0 (-250) $ drawSortedArray arr
            Nothing -> Blank
        , drawHeapButtons world
        , if heapMode world == HeapIdle
          then drawText (-580) 180 0.12 ("Input: " ++ heapValue world)
          else drawText (-580) 180 0.12 ("Step " ++ show (heapCurrentStep world + 1) ++ " / " ++ show (length (heapSteps world)))
        , drawText (-580) 150 0.1 "Max Heap - Array & Tree View"
        , drawHeapStatus world
        ]

drawHeapAsTree :: Heap.Heap Int -> Maybe Int -> Maybe (Int, Int) -> Picture
drawHeapAsTree heap highlightIdx compareIdxs =
    let arr = Heap.toList heap
    in if null arr
       then drawText 0 0 0.15 "Empty Heap"
       else drawHeapTreeNode arr 0 0 0 highlightIdx compareIdxs

drawHeapTreeNode :: [Int] -> Int -> Float -> Float -> Maybe Int -> Maybe (Int, Int) -> Picture
drawHeapTreeNode arr idx x y highlightIdx compareIdxs
    | idx >= length arr = Blank
    | otherwise =
        let value = arr !! idx
            li = Heap.leftChildIndex idx
            ri = Heap.rightChildIndex idx

            isHighlighted = Just idx == highlightIdx
            isComparing = case compareIdxs of
                           Just (i1, i2) -> idx == i1 || idx == i2
                           Nothing -> False

            nodeColor = if isHighlighted
                       then makeColorI 255 140 0 255
                       else if isComparing
                       then makeColorI 255 200 100 255
                       else makeColorI 100 180 100 255

            horizontalSpacing = 300 / (2 ^ (getLevel idx))
            leftX = x - horizontalSpacing
            rightX = x + horizontalSpacing
            childY = y - 80

            leftChild = if li < length arr
                       then Pictures
                           [ drawEdge x y leftX childY
                           , drawHeapTreeNode arr li leftX childY highlightIdx compareIdxs
                           ]
                       else Blank

            rightChild = if ri < length arr
                        then Pictures
                            [ drawEdge x y rightX childY
                            , drawHeapTreeNode arr ri rightX childY highlightIdx compareIdxs
                            ]
                        else Blank
        in Pictures
            [ leftChild
            , rightChild
            , color nodeColor $ translate x y $ circleSolid 25
            , color black $ translate x y $ circle 25
            , centerText x y (show value)
            , translate (x - 30) (y - 35) $ scale 0.08 0.08 $ color (greyN 0.5) $ text ("i=" ++ show idx)
            ]

getLevel :: Int -> Int
getLevel 0 = 0
getLevel n = 1 + getLevel (Heap.parentIndex n)

drawHeapAsArray :: Heap.Heap Int -> Maybe Int -> Maybe (Int, Int) -> Picture
drawHeapAsArray heap highlightIdx compareIdxs =
    let arr = Heap.toList heap
        cellWidth = 60
        startX = -300
    in Pictures $ drawText (-350) 50 0.12 "Array representation:"
                : [drawArrayCell arr i (startX + fromIntegral i * cellWidth) 0 highlightIdx compareIdxs
                   | i <- [0..length arr - 1]]

drawArrayCell :: [Int] -> Int -> Float -> Float -> Maybe Int -> Maybe (Int, Int) -> Picture
drawArrayCell arr idx x y highlightIdx compareIdxs =
    let value = arr !! idx
        isHighlighted = Just idx == highlightIdx
        isComparing = case compareIdxs of
                       Just (i1, i2) -> idx == i1 || idx == i2
                       Nothing -> False
        bgColor = if isHighlighted
                 then makeColorI 255 140 0 255
                 else if isComparing
                 then makeColorI 255 200 100 255
                 else white
    in Pictures
        [ color bgColor $ translate x y $ rectangleSolid 55 40
        , color black $ translate x y $ rectangleWire 55 40
        , translate (x - 10) y $ scale 0.15 0.15 $ color black $ text (show value)
        , translate x (y - 30) $ scale 0.08 0.08 $ color (greyN 0.5) $ text (show idx)
        ]

drawSortedArray :: [Int] -> Picture
drawSortedArray arr =
    let cellWidth = 60
        startX = -300
    in Pictures $ drawText (-350) 50 0.12 "Sorted array (descending):"
                : [drawSortedCell (arr !! i) (startX + fromIntegral i * cellWidth) 0
                   | i <- [0..length arr - 1]]

drawSortedCell :: Int -> Float -> Float -> Picture
drawSortedCell value x y =
    Pictures
        [ color (makeColorI 150 255 150 255) $ translate x y $ rectangleSolid 55 40
        , color black $ translate x y $ rectangleWire 55 40
        , translate (x - 10) y $ scale 0.15 0.15 $ color black $ text (show value)
        ]

drawHeapButtons :: HeapWorld -> Picture
drawHeapButtons world = Pictures
    [ -- Navigation
      drawButton $ Button backButtonBounds "< Back" False

      -- Operations Row 1
    , drawButton $ Button insertButtonBounds "Insert" (heapOperation world == HeapInsert)
    , drawButton $ Button extractButtonBounds "Extract Max" (heapOperation world == HeapExtract)
    , drawButton $ Button heapSortButtonBounds "HeapSort" (heapOperation world == HeapSort)

      -- Replay controls Row 2 - ALWAYS visible
    , drawButton $ Button playPauseButtonBounds (if heapPaused world then "▶ Play" else "⏸ Pause") (heapMode world == HeapAnimating)
    , drawButton $ Button prevStepButtonBounds "◀ Prev" (heapMode world == HeapAnimating)
    , drawButton $ Button nextStepButtonBounds "Next ▶" (heapMode world == HeapAnimating)
    , drawButton $ Button restartButtonBounds "⟲ Restart" (heapMode world == HeapAnimating)
    ]

drawHeapStatus :: HeapWorld -> Picture
drawHeapStatus world
    | heapMode world == HeapAnimating && heapCurrentStep world < length (heapSteps world) =
        let step = heapSteps world !! heapCurrentStep world
        in drawText (-550) (-350) 0.12 (Heap.stepDesc step)
    | heapMode world == HeapIdle =
        let modeText = "Mode: " ++ show (heapOperation world)
            extractText = case heapExtractedValue world of
                Just v -> " | Extracted: " ++ show v
                Nothing -> ""
        in drawText (-550) (-350) 0.12 (modeText ++ extractText)
    | otherwise = Blank

-- ============= HASH TABLE SCREEN =============

initialHashTableWorld :: HashTableWorld
initialHashTableWorld = HashTableWorld
    { htTable = HT.empty 10
    , htSteps = []
    , htCurrentStep = 0
    , htValue = ""
    , htMode = HTIdle
    , htOperation = HTInsert
    , htSearchResult = Nothing
    , htPendingTable = Nothing
    , htPaused = False
    }

drawHashTableWorld :: HashTableWorld -> Picture
drawHashTableWorld world =
    let currentTable = if htMode world == HTAnimating && htCurrentStep world < length (htSteps world)
                       then HT.stepTable (htSteps world !! htCurrentStep world)
                       else htTable world
        currentStep = if htMode world == HTAnimating && htCurrentStep world < length (htSteps world)
                      then Just (htSteps world !! htCurrentStep world)
                      else Nothing
        highlightIdx = case currentStep of
                        Just step -> HT.stepHighlightIndex step
                        Nothing -> Nothing
        highlightValue = case currentStep of
                          Just step -> HT.stepHighlightValue step
                          Nothing -> Nothing
        hashCalc = case currentStep of
                    Just step -> HT.stepHashCalc step
                    Nothing -> Nothing
    in Pictures
        [ translate 0 100 $ drawHashTableArray currentTable highlightIdx highlightValue
        , drawHashTableButtons world
        , if htMode world == HTIdle
          then drawText (-580) 180 0.12 ("Input: " ++ htValue world)
          else drawText (-580) 180 0.12 ("Step " ++ show (htCurrentStep world + 1) ++ " / " ++ show (length (htSteps world)))
        , drawText (-580) 150 0.1 "Hash Table - Chaining Collisions"
        , case hashCalc of
            Just calc -> drawText (-580) (-320) 0.15 calc
            Nothing -> Blank
        , drawHashTableStatus world
        ]

drawHashTableArray :: HT.HashTable Int -> Maybe Int -> Maybe Int -> Picture
drawHashTableArray table highlightIdx highlightValue =
    let buckets = HT.getBuckets table
        size = HT.getSize table
        cellHeight = 50
        cellWidth = 100
        startY = 200
        startX = -200  -- Shifted 100 to the right (was -300)
    in Pictures $ concat
        [ [ -- Draw bucket index (shifted 100 to the right)
            translate (startX) (startY - fromIntegral i * cellHeight) $
                scale 0.12 0.12 $ color black $ text (show i)
          -- Draw bucket cell
          , let isHighlighted = Just i == highlightIdx
                cellColor = if isHighlighted then makeColorI 255 200 100 255 else white
            in Pictures
                [ color cellColor $ translate (startX + 100) (startY - fromIntegral i * cellHeight) $
                    rectangleSolid cellWidth (cellHeight - 5)
                , color black $ translate (startX + 100) (startY - fromIntegral i * cellHeight) $
                    rectangleWire cellWidth (cellHeight - 5)
                ]
          -- Draw chain
          ] ++ drawChain (buckets !! i) (startX + 200) (startY - fromIntegral i * cellHeight) highlightValue
        | i <- [0..size - 1]
        ]

drawChain :: [Int] -> Float -> Float -> Maybe Int -> [Picture]
drawChain [] _ _ _ = []
drawChain (v:vs) x y highlightValue =
    let isHighlighted = Just v == highlightValue
        nodeColor = if isHighlighted then makeColorI 255 140 0 255 else makeColorI 150 200 255 255
        node = Pictures
            [ color nodeColor $ translate x y $ rectangleSolid 35 35
            , color black $ translate x y $ rectangleWire 35 35
            -- Shifted text 10 left and 5 down
            , translate (x - 10) (y - 5) $ scale 0.12 0.12 $ color black $ text (show v)
            ]
        arrow = if not (null vs)
                then color black $ line [(x + 20, y), (x + 35, y)]
                else Blank
    in [node, arrow] ++ drawChain vs (x + 55) y highlightValue

drawHashTableButtons :: HashTableWorld -> Picture
drawHashTableButtons world = Pictures
    [ -- Navigation
      drawButton $ Button backButtonBounds "< Back" False

      -- Operations Row 1
    , drawButton $ Button insertButtonBounds "Insert" (htOperation world == HTInsert)
    , drawButton $ Button searchButtonBounds "Search" (htOperation world == HTSearch)
    , drawButton $ Button deleteButtonBounds "Delete" (htOperation world == HTDelete)

      -- Replay controls Row 2 - ALWAYS visible
    , drawButton $ Button playPauseButtonBounds (if htPaused world then "▶ Play" else "⏸ Pause") (htMode world == HTAnimating)
    , drawButton $ Button prevStepButtonBounds "◀ Prev" (htMode world == HTAnimating)
    , drawButton $ Button nextStepButtonBounds "Next ▶" (htMode world == HTAnimating)
    , drawButton $ Button restartButtonBounds "⟲ Restart" (htMode world == HTAnimating)
    ]

drawHashTableStatus :: HashTableWorld -> Picture
drawHashTableStatus world
    | htMode world == HTAnimating && htCurrentStep world < length (htSteps world) =
        let step = htSteps world !! htCurrentStep world
        in drawText (-550) (-350) 0.12 (HT.stepDesc step)
    | htMode world == HTIdle =
        let modeText = "Mode: " ++ show (htOperation world)
            resultText = case htSearchResult world of
                Just True -> " | Last search: FOUND"
                Just False -> " | Last search: NOT FOUND"
                Nothing -> ""
        in drawText (-550) (-350) 0.12 (modeText ++ resultText)
    | otherwise = Blank

-- ============= APP LOGIC =============

drawApp :: AppState -> Picture
drawApp MenuScreen = drawMenu
drawApp (BSTScreen world) = drawBSTWorld world
drawApp (AVLScreen world) = drawAVLWorld world
drawApp (HeapScreen world) = drawHeapWorld world
drawApp (HashTableScreen world) = drawHashTableWorld world

handleAppEvent :: Event -> AppState -> IO AppState
handleAppEvent (EventKey (MouseButton LeftButton) Down _ (mx, my)) MenuScreen
    | insideButton menuBSTBounds mx my = return (BSTScreen initialBSTWorld)
    | insideButton menuAVLBounds mx my = return (AVLScreen initialAVLWorld)
    | insideButton menuHeapBounds mx my = return (HeapScreen initialHeapWorld)
    | insideButton menuHashTableBounds mx my = return (HashTableScreen initialHashTableWorld)
    | insideButton menuExitBounds mx my = error "EXIT"
    | otherwise = return MenuScreen

handleAppEvent event (BSTScreen world) = do
    result <- handleBSTEvent event world
    case result of
        Left () -> return MenuScreen
        Right newWorld -> return (BSTScreen newWorld)

handleAppEvent event (AVLScreen world) = do
    result <- handleAVLEvent event world
    case result of
        Left () -> return MenuScreen
        Right newWorld -> return (AVLScreen newWorld)

handleAppEvent event (HeapScreen world) = do
    result <- handleHeapEvent event world
    case result of
        Left () -> return MenuScreen
        Right newWorld -> return (HeapScreen newWorld)

handleAppEvent event (HashTableScreen world) = do
    result <- handleHashTableEvent event world
    case result of
        Left () -> return MenuScreen
        Right newWorld -> return (HashTableScreen newWorld)

handleAppEvent _ state = return state

handleBSTEvent :: Event -> BSTWorld -> IO (Either () BSTWorld)
handleBSTEvent (EventKey (MouseButton LeftButton) Down _ (mx, my)) world
    | bstMode world == BSTIdle && insideButton backButtonBounds mx my =
        return (Left ())
    | bstMode world == BSTIdle && insideButton insertButtonBounds mx my =
        return (Right world { bstOperation = BSTInsert, bstSearchResult = Nothing })
    | bstMode world == BSTIdle && insideButton searchButtonBounds mx my =
        return (Right world { bstOperation = BSTSearch, bstSearchResult = Nothing })
    | bstMode world == BSTIdle && insideButton deleteButtonBounds mx my =
        return (Right world { bstOperation = BSTDelete, bstSearchResult = Nothing })
    | bstMode world == BSTIdle && insideButton inOrderButtonBounds mx my =
        return (Right world { bstOperation = BSTInOrder, bstSearchResult = Nothing })
    | bstMode world == BSTIdle && insideButton preOrderButtonBounds mx my =
        return (Right world { bstOperation = BSTPreOrder, bstSearchResult = Nothing })
    | bstMode world == BSTIdle && insideButton postOrderButtonBounds mx my =
        return (Right world { bstOperation = BSTPostOrder, bstSearchResult = Nothing })
    | bstMode world == BSTAnimating && insideButton playPauseButtonBounds mx my =
        return (Right world { bstPaused = not (bstPaused world) })
    | bstMode world == BSTAnimating && insideButton prevStepButtonBounds mx my =
        let newStep = max 0 (bstCurrentStep world - 1)
        in return (Right world { bstCurrentStep = newStep, bstPaused = True })
    | bstMode world == BSTAnimating && insideButton nextStepButtonBounds mx my =
        let newStep = min (length (bstSteps world) - 1) (bstCurrentStep world + 1)
        in return (Right world { bstCurrentStep = newStep, bstPaused = True })
    | bstMode world == BSTAnimating && insideButton restartButtonBounds mx my =
        return (Right world { bstCurrentStep = 0, bstPaused = True })
    | otherwise = return (Right world)

handleBSTEvent (EventKey (Char c) Down _ _) world
    | bstMode world == BSTIdle && c >= '0' && c <= '9' =
        return (Right world { bstValue = bstValue world ++ [c] })
    | otherwise = return (Right world)

handleBSTEvent (EventKey (SpecialKey KeyEnter) Down _ _) world
    | bstMode world == BSTIdle && not (null (bstValue world)) =
        let val = read (bstValue world) :: Int
        in case bstOperation world of
            BSTInsert ->
                let steps = BST.insertSteps val (bstTree world)
                    newTree = BST.insert val (bstTree world)
                in return (Right world
                    { bstSteps = steps
                    , bstCurrentStep = 0
                    , bstValue = ""
                    , bstMode = BSTAnimating
                    , bstSearchResult = Nothing
                    , bstPendingTree = Just newTree
                    , bstPaused = False
                    })
            BSTSearch ->
                let steps = BST.searchSteps val (bstTree world)
                    found = BST.search val (bstTree world)
                in return (Right world
                    { bstSteps = steps
                    , bstCurrentStep = 0
                    , bstValue = ""
                    , bstMode = BSTAnimating
                    , bstSearchResult = Just found
                    , bstPendingTree = Nothing
                    , bstPaused = False
                    })
            BSTDelete ->
                let steps = BST.deleteSteps val (bstTree world)
                    newTree = BST.delete val (bstTree world)
                in return (Right world
                    { bstSteps = steps
                    , bstCurrentStep = 0
                    , bstValue = ""
                    , bstMode = BSTAnimating
                    , bstSearchResult = Nothing
                    , bstPendingTree = Just newTree
                    , bstPaused = False
                    })
            _ -> return (Right world)
    | bstMode world == BSTIdle && (bstOperation world == BSTInOrder || bstOperation world == BSTPreOrder || bstOperation world == BSTPostOrder) =
        case bstOperation world of
            BSTInOrder ->
                let steps = BST.inOrderSteps (bstTree world)
                in return (Right world { bstSteps = steps, bstCurrentStep = 0, bstMode = BSTAnimating, bstPaused = False })
            BSTPreOrder ->
                let steps = BST.preOrderSteps (bstTree world)
                in return (Right world { bstSteps = steps, bstCurrentStep = 0, bstMode = BSTAnimating, bstPaused = False })
            BSTPostOrder ->
                let steps = BST.postOrderSteps (bstTree world)
                in return (Right world { bstSteps = steps, bstCurrentStep = 0, bstMode = BSTAnimating, bstPaused = False })
            _ -> return (Right world)
    | otherwise = return (Right world)

handleBSTEvent (EventKey (SpecialKey KeyBackspace) Down _ _) world
    | bstMode world == BSTIdle && not (null (bstValue world)) =
        return (Right world { bstValue = init (bstValue world) })
    | otherwise = return (Right world)

handleBSTEvent (EventKey (SpecialKey KeyDelete) Down _ _) world
    | bstMode world == BSTIdle = return (Right world { bstValue = "" })
    | otherwise = return (Right world)

handleBSTEvent (EventKey (SpecialKey KeyEsc) Down _ _) world
    | bstMode world == BSTIdle = return (Right world { bstValue = "" })
    | otherwise = return (Right world)

handleBSTEvent _ world = return (Right world)

handleAVLEvent :: Event -> AVLWorld -> IO (Either () AVLWorld)
handleAVLEvent (EventKey (MouseButton LeftButton) Down _ (mx, my)) world
    | avlMode world == AVLIdle && insideButton backButtonBounds mx my =
        return (Left ())
    | avlMode world == AVLIdle && insideButton insertButtonBounds mx my =
        return (Right world { avlOperation = AVLInsert, avlSearchResult = Nothing })
    | avlMode world == AVLIdle && insideButton searchButtonBounds mx my =
        return (Right world { avlOperation = AVLSearch, avlSearchResult = Nothing })
    | avlMode world == AVLIdle && insideButton deleteButtonBounds mx my =
        return (Right world { avlOperation = AVLDelete, avlSearchResult = Nothing })
    | avlMode world == AVLAnimating && insideButton playPauseButtonBounds mx my =
        return (Right world { avlPaused = not (avlPaused world) })
    | avlMode world == AVLAnimating && insideButton prevStepButtonBounds mx my =
        let newStep = max 0 (avlCurrentStep world - 1)
        in return (Right world { avlCurrentStep = newStep, avlPaused = True })
    | avlMode world == AVLAnimating && insideButton nextStepButtonBounds mx my =
        let newStep = min (length (avlSteps world) - 1) (avlCurrentStep world + 1)
        in return (Right world { avlCurrentStep = newStep, avlPaused = True })
    | avlMode world == AVLAnimating && insideButton restartButtonBounds mx my =
        return (Right world { avlCurrentStep = 0, avlPaused = True })
    | otherwise = return (Right world)

handleAVLEvent (EventKey (Char c) Down _ _) world
    | avlMode world == AVLIdle && c >= '0' && c <= '9' =
        return (Right world { avlValue = avlValue world ++ [c] })
    | otherwise = return (Right world)

handleAVLEvent (EventKey (SpecialKey KeyEnter) Down _ _) world
    | avlMode world == AVLIdle && not (null (avlValue world)) =
        let val = read (avlValue world) :: Int
        in case avlOperation world of
            AVLInsert ->
                let steps = AVL.insertSteps val (avlTree world)
                    newTree = AVL.insert val (avlTree world)
                in return (Right world
                    { avlSteps = steps
                    , avlCurrentStep = 0
                    , avlValue = ""
                    , avlMode = AVLAnimating
                    , avlSearchResult = Nothing
                    , avlPendingTree = Just newTree
                    , avlPaused = False
                    })
            AVLSearch ->
                let steps = AVL.searchSteps val (avlTree world)
                    found = AVL.search val (avlTree world)
                in return (Right world
                    { avlSteps = steps
                    , avlCurrentStep = 0
                    , avlValue = ""
                    , avlMode = AVLAnimating
                    , avlSearchResult = Just found
                    , avlPendingTree = Nothing
                    , avlPaused = False
                    })
            AVLDelete ->
                let steps = AVL.deleteSteps val (avlTree world)
                    newTree = AVL.delete val (avlTree world)
                in return (Right world
                    { avlSteps = steps
                    , avlCurrentStep = 0
                    , avlValue = ""
                    , avlMode = AVLAnimating
                    , avlSearchResult = Nothing
                    , avlPendingTree = Just newTree
                    , avlPaused = False
                    })
    | otherwise = return (Right world)

handleAVLEvent (EventKey (SpecialKey KeyBackspace) Down _ _) world
    | avlMode world == AVLIdle && not (null (avlValue world)) =
        return (Right world { avlValue = init (avlValue world) })
    | otherwise = return (Right world)

handleAVLEvent (EventKey (SpecialKey KeyDelete) Down _ _) world
    | avlMode world == AVLIdle = return (Right world { avlValue = "" })
    | otherwise = return (Right world)

handleAVLEvent (EventKey (SpecialKey KeyEsc) Down _ _) world
    | avlMode world == AVLIdle = return (Right world { avlValue = "" })
    | otherwise = return (Right world)

handleAVLEvent _ world = return (Right world)

handleHeapEvent :: Event -> HeapWorld -> IO (Either () HeapWorld)
handleHeapEvent (EventKey (MouseButton LeftButton) Down _ (mx, my)) world
    | heapMode world == HeapIdle && insideButton backButtonBounds mx my =
        return (Left ())
    | heapMode world == HeapIdle && insideButton insertButtonBounds mx my =
        return (Right world { heapOperation = HeapInsert })
    | heapMode world == HeapIdle && insideButton extractButtonBounds mx my =
        return (Right world { heapOperation = HeapExtract })
    | heapMode world == HeapIdle && insideButton heapSortButtonBounds mx my =
        return (Right world { heapOperation = HeapSort })
    | heapMode world == HeapAnimating && insideButton playPauseButtonBounds mx my =
        return (Right world { heapPaused = not (heapPaused world) })
    | heapMode world == HeapAnimating && insideButton prevStepButtonBounds mx my =
        let newStep = max 0 (heapCurrentStep world - 1)
        in return (Right world { heapCurrentStep = newStep, heapPaused = True })
    | heapMode world == HeapAnimating && insideButton nextStepButtonBounds mx my =
        let newStep = min (length (heapSteps world) - 1) (heapCurrentStep world + 1)
        in return (Right world { heapCurrentStep = newStep, heapPaused = True })
    | heapMode world == HeapAnimating && insideButton restartButtonBounds mx my =
        return (Right world { heapCurrentStep = 0, heapPaused = True })
    | otherwise = return (Right world)

handleHeapEvent (EventKey (Char c) Down _ _) world
    | heapMode world == HeapIdle && c >= '0' && c <= '9' && heapOperation world == HeapInsert =
        return (Right world { heapValue = heapValue world ++ [c] })
    | otherwise = return (Right world)

handleHeapEvent (EventKey (SpecialKey KeyEnter) Down _ _) world
    | heapMode world == HeapIdle && heapOperation world == HeapInsert && not (null (heapValue world)) =
        let val = read (heapValue world) :: Int
            steps = Heap.insertSteps val (heapData world)
            newHeap = Heap.insert val (heapData world)
        in return (Right world
            { heapSteps = steps
            , heapCurrentStep = 0
            , heapValue = ""
            , heapMode = HeapAnimating
            , heapPendingHeap = Just newHeap
            , heapExtractedValue = Nothing
            , heapPaused = False
            })
    | heapMode world == HeapIdle && heapOperation world == HeapExtract =
        let steps = Heap.extractSteps (heapData world)
            result = Heap.extractMax (heapData world)
        in case result of
            Nothing -> return (Right world)
            Just (maxVal, newHeap) -> return (Right world
                { heapSteps = steps
                , heapCurrentStep = 0
                , heapMode = HeapAnimating
                , heapPendingHeap = Just newHeap
                , heapExtractedValue = Just maxVal
                , heapPaused = False
                })
    | heapMode world == HeapIdle && heapOperation world == HeapSort =
        let arr = Heap.toList (heapData world)
            steps = Heap.heapSortSteps arr
        in return (Right world
            { heapSteps = steps
            , heapCurrentStep = 0
            , heapMode = HeapAnimating
            , heapPendingHeap = Nothing
            , heapExtractedValue = Nothing
            , heapPaused = False
            })
    | otherwise = return (Right world)

handleHeapEvent (EventKey (SpecialKey KeyBackspace) Down _ _) world
    | heapMode world == HeapIdle && not (null (heapValue world)) =
        return (Right world { heapValue = init (heapValue world) })
    | otherwise = return (Right world)

handleHeapEvent (EventKey (SpecialKey KeyDelete) Down _ _) world
    | heapMode world == HeapIdle = return (Right world { heapValue = "" })
    | otherwise = return (Right world)

handleHeapEvent (EventKey (SpecialKey KeyEsc) Down _ _) world
    | heapMode world == HeapIdle = return (Right world { heapValue = "" })
    | otherwise = return (Right world)

handleHeapEvent _ world = return (Right world)

handleHashTableEvent :: Event -> HashTableWorld -> IO (Either () HashTableWorld)
handleHashTableEvent (EventKey (MouseButton LeftButton) Down _ (mx, my)) world
    | htMode world == HTIdle && insideButton backButtonBounds mx my =
        return (Left ())
    | htMode world == HTIdle && insideButton insertButtonBounds mx my =
        return (Right world { htOperation = HTInsert, htSearchResult = Nothing })
    | htMode world == HTIdle && insideButton searchButtonBounds mx my =
        return (Right world { htOperation = HTSearch, htSearchResult = Nothing })
    | htMode world == HTIdle && insideButton deleteButtonBounds mx my =
        return (Right world { htOperation = HTDelete, htSearchResult = Nothing })
    | htMode world == HTAnimating && insideButton playPauseButtonBounds mx my =
        return (Right world { htPaused = not (htPaused world) })
    | htMode world == HTAnimating && insideButton prevStepButtonBounds mx my =
        let newStep = max 0 (htCurrentStep world - 1)
        in return (Right world { htCurrentStep = newStep, htPaused = True })
    | htMode world == HTAnimating && insideButton nextStepButtonBounds mx my =
        let newStep = min (length (htSteps world) - 1) (htCurrentStep world + 1)
        in return (Right world { htCurrentStep = newStep, htPaused = True })
    | htMode world == HTAnimating && insideButton restartButtonBounds mx my =
        return (Right world { htCurrentStep = 0, htPaused = True })
    | otherwise = return (Right world)

handleHashTableEvent (EventKey (Char c) Down _ _) world
    | htMode world == HTIdle && c >= '0' && c <= '9' =
        return (Right world { htValue = htValue world ++ [c] })
    | otherwise = return (Right world)

handleHashTableEvent (EventKey (SpecialKey KeyEnter) Down _ _) world
    | htMode world == HTIdle && not (null (htValue world)) =
        let val = read (htValue world) :: Int
        in case htOperation world of
            HTInsert ->
                let steps = HT.insertSteps val (htTable world)
                    newTable = HT.insert val (htTable world)
                in return (Right world
                    { htSteps = steps
                    , htCurrentStep = 0
                    , htValue = ""
                    , htMode = HTAnimating
                    , htSearchResult = Nothing
                    , htPendingTable = Just newTable
                    , htPaused = False
                    })
            HTSearch ->
                let steps = HT.searchSteps val (htTable world)
                    found = HT.search val (htTable world)
                in return (Right world
                    { htSteps = steps
                    , htCurrentStep = 0
                    , htValue = ""
                    , htMode = HTAnimating
                    , htSearchResult = Just found
                    , htPendingTable = Nothing
                    , htPaused = False
                    })
            HTDelete ->
                let steps = HT.deleteSteps val (htTable world)
                    newTable = HT.delete val (htTable world)
                in return (Right world
                    { htSteps = steps
                    , htCurrentStep = 0
                    , htValue = ""
                    , htMode = HTAnimating
                    , htSearchResult = Nothing
                    , htPendingTable = Just newTable
                    , htPaused = False
                    })
    | otherwise = return (Right world)

handleHashTableEvent (EventKey (SpecialKey KeyBackspace) Down _ _) world
    | htMode world == HTIdle && not (null (htValue world)) =
        return (Right world { htValue = init (htValue world) })
    | otherwise = return (Right world)

handleHashTableEvent (EventKey (SpecialKey KeyDelete) Down _ _) world
    | htMode world == HTIdle = return (Right world { htValue = "" })
    | otherwise = return (Right world)

handleHashTableEvent (EventKey (SpecialKey KeyEsc) Down _ _) world
    | htMode world == HTIdle = return (Right world { htValue = "" })
    | otherwise = return (Right world)

handleHashTableEvent _ world = return (Right world)

updateApp :: Float -> AppState -> IO AppState
updateApp _ MenuScreen = return MenuScreen
updateApp dt (BSTScreen world) = do
    newWorld <- updateBSTWorld dt world
    return (BSTScreen newWorld)
updateApp dt (AVLScreen world) = do
    newWorld <- updateAVLWorld dt world
    return (AVLScreen newWorld)
updateApp dt (HeapScreen world) = do
    newWorld <- updateHeapWorld dt world
    return (HeapScreen newWorld)
updateApp dt (HashTableScreen world) = do
    newWorld <- updateHashTableWorld dt world
    return (HashTableScreen newWorld)

updateBSTWorld :: Float -> BSTWorld -> IO BSTWorld
updateBSTWorld _ world
    | bstMode world == BSTAnimating && not (bstPaused world) =
        let nextStep = bstCurrentStep world + 1
        in if nextStep >= length (bstSteps world)
           then case bstPendingTree world of
                    Just newTree -> return world
                        { bstMode = BSTIdle
                        , bstCurrentStep = 0
                        , bstSteps = []
                        , bstTree = newTree
                        , bstPendingTree = Nothing
                        , bstPaused = False
                        }
                    Nothing -> return world
                        { bstMode = BSTIdle
                        , bstCurrentStep = 0
                        , bstSteps = []
                        , bstPendingTree = Nothing
                        , bstPaused = False
                        }
           else return world { bstCurrentStep = nextStep }
    | otherwise = return world

updateAVLWorld :: Float -> AVLWorld -> IO AVLWorld
updateAVLWorld _ world
    | avlMode world == AVLAnimating && not (avlPaused world) =
        let nextStep = avlCurrentStep world + 1
        in if nextStep >= length (avlSteps world)
           then case avlPendingTree world of
                    Just newTree -> return world
                        { avlMode = AVLIdle
                        , avlCurrentStep = 0
                        , avlSteps = []
                        , avlTree = newTree
                        , avlPendingTree = Nothing
                        , avlPaused = False
                        }
                    Nothing -> return world
                        { avlMode = AVLIdle
                        , avlCurrentStep = 0
                        , avlSteps = []
                        , avlPendingTree = Nothing
                        , avlPaused = False
                        }
           else return world { avlCurrentStep = nextStep }
    | otherwise = return world

updateHeapWorld :: Float -> HeapWorld -> IO HeapWorld
updateHeapWorld _ world
    | heapMode world == HeapAnimating && not (heapPaused world) =
        let nextStep = heapCurrentStep world + 1
        in if nextStep >= length (heapSteps world)
           then
               let lastSorted = if not (null (heapSteps world))
                               then Heap.stepSortedArray (last (heapSteps world))
                               else Nothing
               in case heapPendingHeap world of
                    Just newHeap -> return world
                        { heapMode = HeapIdle
                        , heapCurrentStep = 0
                        , heapSteps = []
                        , heapData = newHeap
                        , heapPendingHeap = Nothing
                        , heapLastSorted = lastSorted
                        , heapPaused = False
                        }
                    Nothing -> return world
                        { heapMode = HeapIdle
                        , heapCurrentStep = 0
                        , heapSteps = []
                        , heapPendingHeap = Nothing
                        , heapLastSorted = lastSorted
                        , heapPaused = False
                        }
           else return world { heapCurrentStep = nextStep }
    | otherwise = return world

updateHashTableWorld :: Float -> HashTableWorld -> IO HashTableWorld
updateHashTableWorld _ world
    | htMode world == HTAnimating && not (htPaused world) =
        let nextStep = htCurrentStep world + 1
        in if nextStep >= length (htSteps world)
           then case htPendingTable world of
                    Just newTable -> return world
                        { htMode = HTIdle
                        , htCurrentStep = 0
                        , htSteps = []
                        , htTable = newTable
                        , htPendingTable = Nothing
                        , htPaused = False
                        }
                    Nothing -> return world
                        { htMode = HTIdle
                        , htCurrentStep = 0
                        , htSteps = []
                        , htPendingTable = Nothing
                        , htPaused = False
                        }
           else return world { htCurrentStep = nextStep }
    | otherwise = return world