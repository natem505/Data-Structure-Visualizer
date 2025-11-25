module MainMenu
    ( runMainMenu
    ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Common.UI
import qualified DataStructures.BST.BSTVisualizer as BST
import Control.Exception (try, SomeException)

data MenuWorld = MenuWorld
    { menuSelection :: MenuChoice
    }

data MenuChoice = MenuIdle | MenuOpenBST | MenuExit
    deriving (Eq)

-- Button positions
bstButtonBounds :: (Float, Float, Float, Float)
bstButtonBounds = (-200, 50, 200, 120)

exitButtonBounds :: (Float, Float, Float, Float)
exitButtonBounds = (-200, -80, 200, -10)

runMainMenu :: IO ()
runMainMenu =
    playIO
        (InWindow "Data Structure Visualizer" (1200, 800) (100, 100))
        white
        30
        (MenuWorld MenuIdle)
        drawMenu
        handleMenuEvent
        updateMenu

drawMenu :: MenuWorld -> IO Picture
drawMenu _ =
    return $ Pictures
        [ drawTitle (-260) 250 "Data Structure Visualizer"
        , drawButton (Button bstButtonBounds "Binary Search Tree" False)
        , drawButton (Button exitButtonBounds "Exit" False)
        ]

handleMenuEvent :: Event -> MenuWorld -> IO MenuWorld
handleMenuEvent (EventKey (MouseButton LeftButton) Down _ (mx, my)) world
    | insideButton bstButtonBounds mx my = do
        _ <- safeRunBST          -- run visualizer safely
        return (MenuWorld MenuIdle)

    | insideButton exitButtonBounds mx my =
        return (MenuWorld MenuExit)

handleMenuEvent _ world = return world

-- Safely catch *any* exception escaping from BST.runBSTVisualizer
safeRunBST :: IO Bool
safeRunBST = do
    result <- try BST.runBSTVisualizer
    case result of
        Right val -> return val
        Left (_ :: SomeException) -> return True

updateMenu :: Float -> MenuWorld -> IO MenuWorld
updateMenu _ world
    | menuSelection world == MenuExit = do
        putStrLn "Exiting..."
        return world
    | otherwise = return world
