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
    }

data Mode = Idle | Animating
    deriving (Eq)

-- Start with empty tree
initialWorld :: World
initialWorld = World
    { worldTree = Empty
    , worldSteps = []
    , worldCurrentStep = 0
    , worldValue = ""
    , worldMode = Idle
    }

main :: IO ()
main = play
    (InWindow "BST Visualizer" (800, 600) (100, 100))
    white
    2  -- 2 animation steps per second
    initialWorld
    drawWorld
    handleEvent
    updateWorld

-- Render everything: tree + UI text
drawWorld :: World -> Picture
drawWorld world =
    let depth = treeDepth (worldTree world)
        -- Adjust starting y position based on depth
        startY = if depth > 4 then 270 else 200
        -- Adjust initial spacing based on depth
        spacing = calculateSpacing depth
    in Pictures
        [ translate 0 startY $ renderBSTWithHighlight
            (worldTree world)
            (getCurrentHighlight world)
            0 0 spacing
        , translate (-350) 250 $ scale 0.15 0.15 $
            color black $ text "Enter value and press ENTER"
        , translate (-350) 220 $ scale 0.15 0.15 $
            color black $ text ("Current input: " ++ worldValue world)
        , drawStatus world
        ]

-- Calculate tree depth
treeDepth :: BST a -> Int
treeDepth Empty = 0
treeDepth (Node _ l r) = 1 + max (treeDepth l) (treeDepth r)

-- Calculate initial horizontal spacing based on depth - increased values
calculateSpacing :: Int -> Float
calculateSpacing depth
    | depth <= 3 = 200
    | depth == 4 = 180
    | depth == 5 = 160
    | depth == 6 = 140
    | depth == 7 = 120
    | depth == 8 = 100
    | otherwise = 80

-- Show the current step's description
drawStatus :: World -> Picture
drawStatus world
    | worldMode world == Animating && worldCurrentStep world < length (worldSteps world) =
        let step = worldSteps world !! worldCurrentStep world
        in translate (-350) (-250) $ scale 0.12 0.12 $
           color black $ text (stepDesc step)
    | otherwise = Blank

-- Figure out which node to highlight red (the one we're currently examining)
getCurrentHighlight :: World -> Maybe Int
getCurrentHighlight world
    | worldMode world == Animating && worldCurrentStep world < length (worldSteps world) =
        let step = worldSteps world !! worldCurrentStep world
        in extractValue (stepTree step)
    | otherwise = Nothing
  where
    extractValue Empty = Nothing
    extractValue (Node v _ _) = Just v

-- Handle keyboard: digits to type, ENTER to insert, BACKSPACE to delete
handleEvent :: Event -> World -> World
-- Add digits when in Idle mode
handleEvent (EventKey (Char c) Down _ _) world
    | worldMode world == Idle && c >= '0' && c <= '9' =
        world { worldValue = worldValue world ++ [c] }
    | otherwise = world

-- Insert value on ENTER
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) world
    | worldMode world == Idle && not (null (worldValue world)) =
        let val = read (worldValue world) :: Int
            steps = insertSteps val (worldTree world)
            newTree = insert val (worldTree world)
        in world
            { worldTree = newTree
            , worldSteps = steps
            , worldCurrentStep = 0
            , worldValue = ""
            , worldMode = Animating
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
           then world { worldMode = Idle, worldCurrentStep = 0, worldSteps = [] }
           else world { worldCurrentStep = nextStep }
    | otherwise = world