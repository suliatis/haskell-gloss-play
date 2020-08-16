module Main where

import           Graphics.Gloss.Interface.Environment (getScreenSize)
import           Graphics.Gloss.Interface.Pure.Game

data Playground = Playground
  { width  :: Int
  , height :: Int
  }

data Player = Player
  { position  :: (Float, Float)
  , size      :: Float
  , direction :: Direction
  }

maxStep :: Player -> Playground -> Step -> Step
maxStep p pg (Step step) =
  let
    (x, y) = position p
    horizontalStep = min step $ fromIntegral (width pg) / 2 - (size p) / 2 - abs x
    verticalStep = min step $ fromIntegral (height pg) / 2 - (size p) / 2 - abs y
  in
  Step $ case (direction p) of
           Direction'Left  | x < 0 -> horizontalStep
           Direction'Right | x > 0 -> horizontalStep
           Direction'Up    | y > 0 -> verticalStep
           Direction'Down  | y < 0 -> verticalStep
           _                       -> step

data Direction
  = Direction'Left
  | Direction'Down
  | Direction'Up
  | Direction'Right
  | Direction'No

move :: Player -> Step -> Player
move p (Step step) =
  let
    (x, y) = position p
    (x', y') = case (direction p) of
                 Direction'Left  -> (x - step, y)
                 Direction'Down  -> (x, y - step)
                 Direction'Up    -> (x, y + step)
                 Direction'Right -> (x + step, y)
                 Direction'No    -> (x, y)
  in
    p { position = (x', y') }

data World = World
  { playground :: Playground
  , player     :: Player
  }

data Step = Step Float

main :: IO ()
main = do
    (screenWidth, screenHeight) <- getScreenSize
    let world = World { playground = Playground
        { width = screenWidth
        , height = screenHeight
        }
      , player = Player
        { position = (0, 0)
        , size = 20
        , direction = Direction'No
        }
      }
    play FullScreen white 60 world draw update tick

draw :: World -> Picture
draw world = pictures $ [ drawPlayer $ player world ]

drawPlayer :: Player -> Picture
drawPlayer (Player (x, y) s _) =
  translate x y $ color black $ rectangleSolid s s

update :: Event -> World -> World
update (EventKey (SpecialKey key) keyState _ _) world =
  let
    direction' = case (key, keyState, direction $ player world) of
             (KeyLeft, Down, _)              -> Direction'Left
             (KeyLeft, Up, Direction'Left)   -> Direction'No
             (KeyDown, Down, _)              -> Direction'Down
             (KeyDown, Up, Direction'Down)   -> Direction'No
             (KeyUp, Down, _)                -> Direction'Up
             (KeyUp, Up, Direction'Up)       -> Direction'No
             (KeyRight, Down, _)             -> Direction'Right
             (KeyRight, Up, Direction'Right) -> Direction'No
             (_, _, currentDirection)        -> currentDirection
    player' = (player world) { direction = direction' }
  in
    world { player = player' }
update _ world = world

tick :: Float -> World -> World
tick _ (World pg p) =
  let
    step = maxStep p pg $ Step 10
    p' = move p step
  in
    World pg p'
