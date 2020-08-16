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
  , step      :: Step
  }

isLeaving :: Player -> Playground -> Bool
isLeaving p pg =
  let
    (x, y) = position p
  in
    x > fromIntegral (width pg) / 2 - (size p) / 2
    || x < -(fromIntegral (width pg) / 2 - (size p) / 2)
    || y > fromIntegral (height pg) / 2 - (size p) / 2
    || y < -(fromIntegral (height pg) / 2 - (size p) / 2)

data Direction
  = Direction'Left
  | Direction'Down
  | Direction'Up
  | Direction'Right
  | Direction'No

move :: Player -> Player
move p =
  let
    (x, y) = position p
    Step stepValue = step p
    (x', y') = case (direction p) of
                 Direction'Left  -> (x - stepValue, y)
                 Direction'Down  -> (x, y - stepValue)
                 Direction'Up    -> (x, y + stepValue)
                 Direction'Right -> (x + stepValue, y)
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
        , step = Step 5
        }
      }
    play FullScreen white 60 world draw update tick

draw :: World -> Picture
draw world = pictures $ [ drawPlayer $ player world ]

drawPlayer :: Player -> Picture
drawPlayer (Player (x, y) s _ _) =
  translate x y $ color black $ rectangleSolid s s

update :: Event -> World -> World
update (EventKey (SpecialKey key) keyState _ _) world =
  let
    direction' = case (key, keyState) of
             (KeyLeft, Down)  -> Direction'Left
             (KeyDown, Down)  -> Direction'Down
             (KeyUp, Down)    -> Direction'Up
             (KeyRight, Down) -> Direction'Right
             (_, _)           -> Direction'No
    player' = (player world) { direction = direction' }
  in
    world { player = player' }
update _ world = world

tick :: Float -> World -> World
tick _ (World pg p) =
  let
    p' = if isLeaving (move p) pg then p else move p
  in
    World pg p'
