module DataStructures.BST.BSTVisualizer
    ( runBSTVisualizer
    ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import DataStructures.BST.BST
import Common.UI
import Control.Exception (catch, throwIO, Exception)
import Data.Typeable (Typeable)

data BSTWorld = BSTWorld
    { bstTree :: BST Int
    , bstSteps :: [Step Int]
    , bstCurrentStep :: Int
    , bstValue :: String
    , bstMode :: BSTMode
    , bstOperation :: BSTOperation
    , bstSearchResult :: Maybe Bool
    , bstPendingTree :: Maybe (BST Int)
    }

data BSTMode = BSTIdle | BSTAnimating
    deriving (Eq)

data BSTOperation = BSTInsert | BSTSearch | BSTDelete
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

-- Exception to signal back button pressed
data BackToMenu = BackToMenu deriving (Show, Typeable)
instance Exception BackToMenu

initialBSTWorld :: BSTWorld
initialBSTWorld = BSTWorld
    { bstTree = Empty
    , bstSteps = []
    , bstCurrentStep = 0
    , bstValue = ""
    , bstMode = BSTIdle
    , bstOperation = BSTInsert
    , bstSearchResult = Nothing
    , bstPendingTree = Nothing
    }

backButtonBounds :: (Float, Float, Float, Float)
backButtonBounds = (-550, 360, -450, 390)

insertButtonBounds :: (Float, Float, Float, Float)
insertButtonBounds = (-550, 320, -450, 350)

searchButtonBounds :: (Float, Float, Float, Float)
searchButtonBounds = (-440, 320, -340, 350)

deleteButtonBounds :: (Float, Float, Float, Float)
deleteButtonBounds = (-330, 320, -230, 350)

runBSTVisualizer :: IO Bool
runBSTVisualizer = do
    catch (do
        playIO
            (InWindow "BST Visualizer" (1200, 800) (100, 100))
            white
            1
            initialBSTWorld
            (return . drawBSTWorld)
            handleBSTEventIO
            updateBSTWorldIO
        return False  -- Normal exit (window closed)
        ) (\BackToMenu -> return True)  -- Back button pressed

handleBSTEventIO :: Event -> BSTWorld -> IO BSTWorld
handleBSTEventIO event world = do
    let newWorld = handleBSTEvent event world
    -- Check if back button was pressed
    case event of
        EventKey (MouseButton LeftButton) Down _ (mx, my)
            | bstMode world == BSTIdle && insideButton backButtonBounds mx my ->
                throwIO BackToMenu
        _ -> return newWorld

updateBSTWorldIO :: Float -> BSTWorld -> IO BSTWorld
updateBSTWorldIO dt world = return (updateBSTWorld dt world)

drawBSTWorld :: BSTWorld -> Picture
drawBSTWorld world =
    let currentTree = if bstMode world == BSTAnimating && bstCurrentStep world < length (bstSteps world)
                      then stepTree (bstSteps world !! bstCurrentStep world)
                      else bstTree world
    in Pictures
        [ translate 0 200 $ renderBSTWithHighlight
            currentTree
            (getCurrentBSTHighlight world)
            (getBSTHighlightColor world)
        , drawBSTButtons world
        , drawText (-550) 280 0.12 ("Input: " ++ bstValue world)
        , drawText (-550) 250 0.1 "Click button to select operation"
        , drawBSTStatus world
        ]

renderBSTWithHighlight :: (Show a, Eq a) => BST a -> Maybe a -> Color -> Picture
renderBSTWithHighlight tree highlight highlightColor =
    let positioned = layoutTree tree
    in drawPositioned positioned highlight highlightColor

layoutTree :: BST a -> Maybe (Positioned a)
layoutTree Empty = Nothing
layoutTree tree = Just (positionTree tree 0 0)

minNodeSpacing :: Float
minNodeSpacing = 60

positionTree :: BST a -> Float -> Float -> Positioned a
positionTree Empty _ _ = error "Cannot position Empty tree"
positionTree (Node value left right) x y =
    let
        leftTree = case left of
            Empty -> Nothing
            l -> Just (positionTree l 0 (y - 90))

        rightTree = case right of
            Empty -> Nothing
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

drawBSTButtons :: BSTWorld -> Picture
drawBSTButtons world = Pictures
    [ drawButton $ Button backButtonBounds "< Back" False
    , drawButton $ Button insertButtonBounds "Insert" (bstOperation world == BSTInsert)
    , drawButton $ Button searchButtonBounds "Search" (bstOperation world == BSTSearch)
    , drawButton $ Button deleteButtonBounds "Delete" (bstOperation world == BSTDelete)
    ]

drawBSTStatus :: BSTWorld -> Picture
drawBSTStatus world
    | bstMode world == BSTAnimating && bstCurrentStep world < length (bstSteps world) =
        let step = bstSteps world !! bstCurrentStep world
        in drawText (-550) (-350) 0.12 (stepDesc step)
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
        stepHighlight (bstSteps world !! bstCurrentStep world)
    | otherwise = Nothing

handleBSTEvent :: Event -> BSTWorld -> BSTWorld
handleBSTEvent (EventKey (MouseButton LeftButton) Down _ (mx, my)) world
    | bstMode world == BSTIdle && insideButton insertButtonBounds mx my =
        world { bstOperation = BSTInsert, bstSearchResult = Nothing }
    | bstMode world == BSTIdle && insideButton searchButtonBounds mx my =
        world { bstOperation = BSTSearch, bstSearchResult = Nothing }
    | bstMode world == BSTIdle && insideButton deleteButtonBounds mx my =
        world { bstOperation = BSTDelete, bstSearchResult = Nothing }
    | otherwise = world

handleBSTEvent (EventKey (Char c) Down _ _) world
    | bstMode world == BSTIdle && c >= '0' && c <= '9' =
        world { bstValue = bstValue world ++ [c] }
    | otherwise = world

handleBSTEvent (EventKey (SpecialKey KeyEnter) Down _ _) world
    | bstMode world == BSTIdle && not (null (bstValue world)) =
        let val = read (bstValue world) :: Int
        in case bstOperation world of
            BSTInsert ->
                let steps = insertSteps val (bstTree world)
                    newTree = insert val (bstTree world)
                in world
                    { bstSteps = steps
                    , bstCurrentStep = 0
                    , bstValue = ""
                    , bstMode = BSTAnimating
                    , bstSearchResult = Nothing
                    , bstPendingTree = Just newTree
                    }
            BSTSearch ->
                let steps = searchSteps val (bstTree world)
                    found = search val (bstTree world)
                in world
                    { bstSteps = steps
                    , bstCurrentStep = 0
                    , bstValue = ""
                    , bstMode = BSTAnimating
                    , bstSearchResult = Just found
                    , bstPendingTree = Nothing
                    }
            BSTDelete ->
                let steps = deleteSteps val (bstTree world)
                    newTree = delete val (bstTree world)
                in world
                    { bstSteps = steps
                    , bstCurrentStep = 0
                    , bstValue = ""
                    , bstMode = BSTAnimating
                    , bstSearchResult = Nothing
                    , bstPendingTree = Just newTree
                    }
    | otherwise = world

handleBSTEvent (EventKey (SpecialKey KeyBackspace) Down _ _) world
    | bstMode world == BSTIdle && not (null (bstValue world)) =
        world { bstValue = init (bstValue world) }
    | otherwise = world

handleBSTEvent (EventKey (SpecialKey KeyDelete) Down _ _) world
    | bstMode world == BSTIdle = world { bstValue = "" }
    | otherwise = world

handleBSTEvent (EventKey (SpecialKey KeyEsc) Down _ _) world
    | bstMode world == BSTIdle = world { bstValue = "" }
    | otherwise = world

handleBSTEvent _ world = world

updateBSTWorld :: Float -> BSTWorld -> BSTWorld
updateBSTWorld _ world
    | bstMode world == BSTAnimating =
        let nextStep = bstCurrentStep world + 1
        in if nextStep >= length (bstSteps world)
           then case bstPendingTree world of
                    Just newTree -> world
                        { bstMode = BSTIdle
                        , bstCurrentStep = 0
                        , bstSteps = []
                        , bstTree = newTree
                        , bstPendingTree = Nothing
                        }
                    Nothing -> world
                        { bstMode = BSTIdle
                        , bstCurrentStep = 0
                        , bstSteps = []
                        , bstPendingTree = Nothing
                        }
           else world { bstCurrentStep = nextStep }
    | otherwise = world