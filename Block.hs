module Block where

import Graphics.Gloss
import System.Random
import Data.Fixed

import Tile
import KeyPress

data BlockType = LineBlock | SquareBlock | LBlock | JBlock | TBlock | SBlock | ZBlock deriving (Enum, Eq, Show, Bounded)

data Block = Block
  {
    blockType :: BlockType,
    col       :: Color,
    rotation  :: Float,
    tiles     :: [Tile]
  } deriving Show

createBlock :: BlockType -> Float -> (Float, Float) -> Block
createBlock t r pos = Block {
  blockType = t,
  col       = getBlockColor t,
  rotation  = r,
  tiles     = getBlockTiles t r pos
}

recreateBlock :: Block -> Block
recreateBlock b = createBlock (blockType b) (rotation b) (pos ((tiles b)!!0))

randomBlock :: StdGen -> (Float, Float) -> (Block, StdGen)
randomBlock gen pos = (createBlock t (fromIntegral r) pos, gen'')
  where 
    (t, gen')  = randomBlockType gen
    (r, gen'') = randomRotation  gen'

randomBlockType :: StdGen -> (BlockType, StdGen)
randomBlockType g = (toEnum $ r, g') where (r, g') = randomR (0,6) g

randomRotation :: StdGen -> (Int, StdGen)
randomRotation g = (90*r, g') where (r, g') = randomR (0,3) g

rotateBlock :: Block -> Block
rotateBlock b = if hasBlockLeftBounds newBlock then b else newBlock
  where newBlock = recreateBlock $ b { rotation = ((rotation b) + 90) `mod'` 360 }

getBlockColor :: BlockType -> Color
getBlockColor LineBlock = blue
getBlockColor SquareBlock = yellow
getBlockColor LBlock = red
getBlockColor JBlock = dark $ green
getBlockColor TBlock = green
getBlockColor SBlock = orange
getBlockColor ZBlock = dark $ red

getBlockTiles :: BlockType -> Float -> (Float, Float) -> [Tile]
getBlockTiles LineBlock r pos     = createBlockLine pos offset 4 where offset = if (r `mod'` 180 == 0) then (tileSize, 0) else (0, tileSize)
getBlockTiles SquareBlock r (x,y) = createBlockLine (x,y) (tileSize, 0) 2 ++ createBlockLine (x,y+tileSize) (tileSize, 0) 2 
getBlockTiles LBlock r (x,y) = createBlockLine (x,y) offset 3 ++ [createTile (f x r 90) (f y r 180)]
  where offset = if (r `mod'` 180 == 0) then (0, tileSize) else (tileSize, 0)
        f p r o = p + (2 - (((r+o) `mod'` 360) / 90))*tileSize

getBlockTiles JBlock r (x,y) = createBlockLine (x,y) offset 3 ++ [createTile (f x r 0) (f y r 90)]
  where offset = if (r `mod'` 180 == 0) then (0, tileSize) else (tileSize, 0)
        f p r o = p + ((((r+o) `mod'` 360) / 90) - 1)*tileSize

getBlockTiles TBlock r (x,y) = createBlockLine (x,y) offset 3 ++ [createTile (f x r 90) (f y r 0)]
  where offset = if (r `mod'` 180 == 0) then (tileSize, 0) else (0, tileSize)
        f p r ex = if r == ex then p-tileSize else p+tileSize

getBlockTiles SBlock r (x,y) = createBlockLine (x,y) offset 2 ++ createBlockLine (x+tileSize, yo) offset 2
  where offset = if r `mod'` 180 == 0 then (tileSize, 0) else (0, tileSize)
        yo     = if r `mod'` 180 == 0 then y+tileSize else y-tileSize

getBlockTiles ZBlock r (x,y) = createBlockLine (x,y) offset 2 ++ createBlockLine (x-tileSize, yo) offset 2
  where offset = if r `mod'` 180 == 0 then (tileSize, 0) else (0, tileSize)
        yo     = if r `mod'` 180 == 0 then y+tileSize else y-tileSize

createBlockLine :: (Float, Float) -> (Float, Float) -> Int -> [Tile]
createBlockLine _ _ 0 = []
createBlockLine (x,y) (x',y') l = createTile x y : createBlockLine (x+x',y+y') (x',y') (l-1)

moveBlocksDown :: [Block] -> Float -> [Block]
moveBlocksDown bs y = map (\b -> b { tiles = moveTilesDown (tiles b) y}) bs

clearBlocks :: [Block] -> Float -> [Block]
clearBlocks bs y = map (\b -> b { tiles = clearTiles (tiles b) y}) bs

-- Collision detection
hasBlockLeftBounds :: Block -> Bool
hasBlockLeftBounds b = any hasTileLeftBounds (tiles b)

hasBlockHitAnotherBlock :: KeyPress -> Block -> Block -> Bool
hasBlockHitAnotherBlock kp cb lb = any (hasTileHitAnotherTile kp (tiles cb)) (tiles lb)

hasBlockHitLeftSide :: Block -> Bool
hasBlockHitLeftSide b = any hasTileHitLeftSide (tiles b)

hasBlockHitRightSide :: Block -> Bool
hasBlockHitRightSide b = any hasTileHitRightSide (tiles b)

hasBlockLanded :: Block -> Bool
hasBlockLanded b = any hasTileLanded (tiles b)

-- Render functions
renderBlock :: Block -> Picture
renderBlock b = pictures $ renderTiles (tiles b) (col b)
