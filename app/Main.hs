module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import BST
import Render

-- Animation state tracks everything about our visualizer
data World = World
    { worldTree :: BST Int           -- the actual BST
    , worldSteps :: [Step Int]       -- steps from insertSteps
    , worldCurrentStep :: Int        -- which step we're on
    , worldValue :: String           -- user's typed input
    , worldMode :: Mode              -- are we animating or idle?
    , worldOperation :: Operation    -- current operation mode
    , worldSearchResult :: Maybe Bool -- result of search (Just True/False or Nothing)
    , worldPendingTree :: Maybe (BST Int)  -- tree to apply after animation completes
    }

data Mode = Idle | Animating
    deriving (Eq)

data Operation = Insert | Search | Delete
    deriving (Eq, Show)

-- Start with empty tree in Insert mode
initialWorld :: World
initialWorld = World
    { worldTree = Empty
    , worldSteps = []
    , worldCurrentStep = 0
    , worldValue = ""
    , worldMode = Idle
    , worldOperation = Insert
    , worldSearchResult = Nothing
    , worldPendingTree = Nothing
    }

main :: IO ()
main = play
    (InWindow "BST Visualizer" (1200, 800) (100, 100))
    white
    1  -- 1 step per second
    initialWorld
    drawWorld
    handleEvent
    updateWorld

-- Button dimensions and positions
insertButtonBounds :: (Float, Float, Float, Float)  -- (x1, y1, x2, y2)
insertButtonBounds = (-550, 340, -450, 370)

searchButtonBounds :: (Float, Float, Float, Float)
searchButtonBounds = (-440, 340, -340, 370)

deleteButtonBounds :: (Float, Float, Float, Float)
deleteButtonBounds = (-330, 340, -230, 370)

-- Check if point is inside button bounds
insideButton :: (Float, Float, Float, Float) -> Float -> Float -> Bool
insideButton (x1, y1, x2, y2) px py =
    px >= x1 && px <= x2 && py >= y1 && py <= y2

-- Render everything: tree + UI
drawWorld :: World -> Picture
drawWorld world =
    let currentTree = if worldMode world == Animating && worldCurrentStep world < length (worldSteps world)
                      then stepTree (worldSteps world !! worldCurrentStep world)
                      else worldTree world
    in Pictures
        [ translate 0 250 $ renderBSTWithHighlight
            currentTree
            (getCurrentHighlight world)
            (getHighlightColor world)
        , drawButtons (worldOperation world)
        , translate (-550) 300 $ scale 0.12 0.12 $
            color black $ text ("Input: " ++ worldValue world)
        , translate (-550) 270 $ scale 0.12 0.12 $
            color (greyN 0.5) $ text "Click button to select operation"
        , drawStatus world
        ]

-- Get highlight color based on operation
getHighlightColor :: World -> Color
getHighlightColor world = case worldOperation world of
    Insert -> makeColorI 220 50 50 255   -- red for insert
    Search -> makeColorI 255 165 0 255   -- orange for search
    Delete -> makeColorI 180 50 50 255   -- dark red for delete

-- Draw the three buttons
drawButtons :: Operation -> Picture
drawButtons currentOp = Pictures
    [ drawButton insertButtonBounds "Insert" (currentOp == Insert)
    , drawButton searchButtonBounds "Search" (currentOp == Search)
    , drawButton deleteButtonBounds "Delete" (currentOp == Delete)
    ]

-- Draw a single button (highlighted if selected)
drawButton :: (Float, Float, Float, Float) -> String -> Bool -> Picture
drawButton (x1, y1, x2, y2) label isSelected =
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

-- Show the current step's description
drawStatus :: World -> Picture
drawStatus world
    | worldMode world == Animating && worldCurrentStep world < length (worldSteps world) =
        let step = worldSteps world !! worldCurrentStep world
        in translate (-550) (-350) $ scale 0.12 0.12 $
           color black $ text (stepDesc step)
    | worldMode world == Idle =
        let modeText = "Mode: " ++ show (worldOperation world)
            resultText = case worldSearchResult world of
                Just True -> " | Last search: FOUND"
                Just False -> " | Last search: NOT FOUND"
                Nothing -> ""
        in translate (-550) (-350) $ scale 0.12 0.12 $
           color (greyN 0.5) $ text (modeText ++ resultText)
    | otherwise = Blank

-- Figure out which node to highlight (from the step's highlight field)
getCurrentHighlight :: World -> Maybe Int
getCurrentHighlight world
    | worldMode world == Animating && worldCurrentStep world < length (worldSteps world) =
        stepHighlight (worldSteps world !! worldCurrentStep world)
    | otherwise = Nothing

-- Handle events
handleEvent :: Event -> World -> World

-- Mouse clicks on buttons
handleEvent (EventKey (MouseButton LeftButton) Down _ (mx, my)) world
    | worldMode world == Idle =
        if insideButton insertButtonBounds mx my
        then world { worldOperation = Insert, worldSearchResult = Nothing }
        else if insideButton searchButtonBounds mx my
        then world { worldOperation = Search, worldSearchResult = Nothing }
        else if insideButton deleteButtonBounds mx my
        then world { worldOperation = Delete, worldSearchResult = Nothing }
        else world
    | otherwise = world

-- Add digits when in Idle mode
handleEvent (EventKey (Char c) Down _ _) world
    | worldMode world == Idle && c >= '0' && c <= '9' =
        world { worldValue = worldValue world ++ [c] }
    | otherwise = world

-- Execute operation on ENTER
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) world
    | worldMode world == Idle && not (null (worldValue world)) =
        let val = read (worldValue world) :: Int
        in case worldOperation world of
            Insert ->
                let steps = insertSteps val (worldTree world)
                    newTree = insert val (worldTree world)
                in world
                    { worldSteps = steps
                    , worldCurrentStep = 0
                    , worldValue = ""
                    , worldMode = Animating
                    , worldSearchResult = Nothing
                    , worldPendingTree = Just newTree
                    }
            Search ->
                let steps = searchSteps val (worldTree world)
                    found = search val (worldTree world)
                in world
                    { worldSteps = steps
                    , worldCurrentStep = 0
                    , worldValue = ""
                    , worldMode = Animating
                    , worldSearchResult = Just found
                    , worldPendingTree = Nothing
                    }
            Delete ->
                let steps = deleteSteps val (worldTree world)
                    newTree = delete val (worldTree world)
                in world
                    { worldSteps = steps
                    , worldCurrentStep = 0
                    , worldValue = ""
                    , worldMode = Animating
                    , worldSearchResult = Nothing
                    , worldPendingTree = Just newTree
                    }
    | otherwise = world

-- Delete last character with BACKSPACE
handleEvent (EventKey (SpecialKey KeyBackspace) Down _ _) world
    | worldMode world == Idle && not (null (worldValue world)) =
        world { worldValue = init (worldValue world) }
    | otherwise = world

-- Clear entire input with DELETE or Escape
handleEvent (EventKey (SpecialKey KeyDelete) Down _ _) world
    | worldMode world == Idle =
        world { worldValue = "" }
    | otherwise = world

handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) world
    | worldMode world == Idle =
        world { worldValue = "" }
    | otherwise = world

handleEvent _ world = world

-- Advance animation: move to next step, or return to Idle when done
updateWorld :: Float -> World -> World
updateWorld _ world
    | worldMode world == Animating =
        let nextStep = worldCurrentStep world + 1
        in if nextStep >= length (worldSteps world)
           then -- Animation complete, apply pending tree update
                case worldPendingTree world of
                    Just newTree -> world
                        { worldMode = Idle
                        , worldCurrentStep = 0
                        , worldSteps = []
                        , worldTree = newTree
                        , worldPendingTree = Nothing
                        }
                    Nothing -> world
                        { worldMode = Idle
                        , worldCurrentStep = 0
                        , worldSteps = []
                        , worldPendingTree = Nothing
                        }
           else world { worldCurrentStep = nextStep }
    | otherwise = world