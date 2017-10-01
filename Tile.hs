module Tile where

import Graphics.Gloss

import KeyPress

data Tile = Tile
  {
    pos :: (Float, Float)
  } deriving Show

tileSize = 20 :: Float

createTile :: Float -> Float -> Tile
createTile x y = Tile { pos = (x, y) }

clearTiles :: [Tile] -> Float -> [Tile]
clearTiles ts y = filter (\t -> (snd (pos t)) /= y) ts

moveTiles :: [Tile] -> KeyPress -> [Tile]
moveTiles ts kp = map (moveTile kp) ts

moveTile :: KeyPress -> Tile -> Tile
moveTile kp t = t { pos = (x, y) }
  where (x,y) = addPos (pos t) (keypressToDir kp)

keypressToDir :: KeyPress -> (Float, Float)
keypressToDir kp 
 | kp == South = (0, -tileSize)
 | kp == East  = (tileSize, 0)
 | kp == West  = (-tileSize, 0)
 | kp == None  = (0, 0)

moveTilesDown :: [Tile] -> Float -> [Tile]
moveTilesDown ts y = map (\t -> if snd (pos t) > y then t { pos = addPos (pos t) (0, -tileSize) } else t) ts

-- Render functions
renderTiles :: [Tile] -> Color -> [Picture]
renderTiles [] _     = []
renderTiles (t:ts) c = renderTile t c : renderTiles ts c

renderTile :: Tile -> Color -> Picture
renderTile t c = 
  color c $ translate x y $ rectangleSolid s s
  where
    (x, y) = (pos t)
    s      = tileSize - 1

-- Collision detection
hasTileLeftBounds :: Tile -> Bool
hasTileLeftBounds t = x < -180 || x > 180 || y < -290 where (x, y) = pos t 

hasTileLanded :: Tile -> Bool
hasTileLanded t  = y <= -290 where (x, y) = pos t

hasTileHitLeftSide :: Tile -> Bool
hasTileHitLeftSide t = (x <= -180) where (x, y) = pos t

hasTileHitRightSide :: Tile -> Bool
hasTileHitRightSide t = (x >= 180) where (x, y) = pos t

hasTileHitAnotherTile :: KeyPress -> [Tile] -> Tile -> Bool
hasTileHitAnotherTile kp cb_ts lb_t = any (hasTileHitTile kp lb_t) cb_ts

hasTileHitTile :: KeyPress -> Tile -> Tile -> Bool
hasTileHitTile kp lb_t cb_t = isCollision kp
  where (x,  y ) = (pos lb_t)
        (x', y') = (pos cb_t)
        isCollision West  = y == y' && x+tileSize == x'
        isCollision East  = y == y' && x-tileSize == x'
	isCollision South = x == x' && y+tileSize == y'

addPos :: Num a => (a, a) -> (a, a) -> (a, a)
addPos (a, b) (c, d) = (a+c, b+d)
