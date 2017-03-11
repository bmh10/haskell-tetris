module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.IO
import System.Random
import Control.Monad
import Data.Fixed
import Data.List
import Data.Maybe

fps = 5
tileSize = 20
width = 400
height = 600
offset = 100
initialBrickPos = (10, 210)
window = InWindow "Tetris" (width, height) (offset, offset)
background = black

data BlockType = LineBlock | SquareBlock | LBlock | TBlock | SBlock | ZBlock 
                 deriving (Enum, Eq, Show, Bounded)
data KeyPress  = South | East | West | None deriving (Eq, Show)

data Tile = Tile
  {
    pos :: (Float, Float)
  } deriving Show

createTile x y = Tile { pos = (x, y) }

data Block = Block
  {
    blockType :: BlockType,
    col       :: Color,
    rotation  :: Float,
    tiles     :: [Tile]
  } deriving Show

-- TODO: pos hack
recreateBlock b = createBlock (blockType b) (rotation b) (pos ((tiles b)!!0))

randomBlock :: StdGen -> (Float, Float) -> (Block, StdGen)
randomBlock gen pos = (createBlock t (fromIntegral r) pos, gen'')
  where 
    (t, gen')  = randomBlockType gen
    (r, gen'') = randomRotation  gen'   

createBlock t r pos = Block {
  blockType = t,
  col       = getBlockColor t,
  rotation  = r,
  tiles     = getBlockTiles t r pos
}

getBlockColor :: BlockType -> Color
getBlockColor LineBlock = blue
getBlockColor SquareBlock = yellow
getBlockColor LBlock = red

getBlockTiles :: BlockType -> Float -> (Float, Float) -> [Tile]
getBlockTiles LineBlock r pos     = createBlockLine pos offset 4 where offset = if (r `mod'` 180 == 0) then (tileSize, 0) else (0, tileSize)
getBlockTiles SquareBlock r (x,y) = createBlockLine (x,y) (tileSize, 0) 2 ++ createBlockLine (x,y+tileSize) (tileSize, 0) 2 
getBlockTiles LBlock r (x,y)
 | r == 0   = createBlockLine (x,y) (0, tileSize) 3 ++ [createTile (x+tileSize) y]
 | r == 90  = createBlockLine (x,y) (tileSize, 0) 3 ++ [createTile x (y-tileSize)]
 | r == 180 = createBlockLine (x,y) (0, tileSize) 3 ++ [createTile (x-tileSize) (y+2*tileSize)]
 | r == 270 = createBlockLine (x,y) (tileSize, 0) 3 ++ [createTile (x+2*tileSize) (y+tileSize)] 

randomBlockType :: StdGen -> (BlockType, StdGen)
randomBlockType g = (toEnum $ r, g') where (r, g') = randomR (2,2) g

randomRotation :: StdGen -> (Int, StdGen)
randomRotation g = (90*r, g') where (r, g') = randomR (0,3) g

moveBlock :: TetrisGame -> KeyPress -> TetrisGame
moveBlock g kp 
 | (hasBlockLanded g) || (hasBlockHitLeftSide g && kp == West) || (hasBlockHitRightSide g && kp == East) = g
 | otherwise = g { currentBlock = b { tiles = ts } }
  where 
    b = currentBlock g
    ts = moveTiles (tiles b) kp

moveTiles ts kp = map (moveTile kp) ts

moveTile kp t = t { pos = (x, y) }
  where (x,y) = (pos t) + (keypressToDir kp)

keypressToDir kp 
 | kp == South = (0, -tileSize)
 | kp == East  = (tileSize, 0)
 | kp == West  = (-tileSize, 0)
 | kp == None  = (0, 0)

hasBlockCollided g kp = any (hasBlockHitAnotherBlock kp (currentBlock g)) (landedBlocks g)
hasBlockHitAnotherBlock kp cb lb = any (hasTileHitAnotherTile kp (tiles cb)) (tiles lb)
hasTileHitAnotherTile kp cb_ts lb_t = any (hasTileHitTile kp lb_t) cb_ts
hasTileHitTile kp lb_t cb_t = isCollision kp
  where (x,  y ) = (pos lb_t)
        (x', y') = (pos cb_t)
        isCollision West  = y == y' && x+tileSize == x'
        isCollision East  = y == y' && x-tileSize == x'
	isCollision South = x == x' && y+tileSize == y'

hasBlockLanded g = any hasTileLanded (tiles (currentBlock g)) || hasBlockCollided g South
hasTileLanded t  = y <= -290 where (x, y) = pos t

hasBlockHitLeftSide g = any hasTileHitLeftSide (tiles (currentBlock g)) || hasBlockCollided g West
hasTileHitLeftSide t = (x <= -180) where (x, y) = pos t

hasBlockHitRightSide g = any hasTileHitRightSide (tiles (currentBlock g)) || hasBlockCollided g East
hasTileHitRightSide t = (x >= 180) where (x, y) = pos t

createBlockLine :: (Float, Float) -> (Float, Float) -> Int -> [Tile]
createBlockLine _ _ 0 = []
createBlockLine (x,y) (x',y') l = createTile x y : createBlockLine (x+x',y+y') (x',y') (l-1)

data TetrisGame = Game
  {
    currentBlock :: Block,
    landedBlocks :: [Block],
    keyPress :: KeyPress,
    score :: Int,
    gen :: StdGen
  } deriving Show

initGame = do
  stdGen <- newStdGen
  let (initialBlock, gen') = randomBlock stdGen initialBrickPos
  let initialState = Game {
      currentBlock = initialBlock,
      landedBlocks = [],
      keyPress = None,
      score = 0,
      gen = gen'
    }
  return initialState

render :: TetrisGame -> Picture 
--render g = pictures [translate x y $ rotate (brickRotation g) $ translate (-x) (-y) $ renderBrick g, renderDashboard g]
render g = pictures [renderDashboard g, renderBlocks g]
  --where (x, y) = brickPos g

renderBlocks :: TetrisGame -> Picture
renderBlocks g = pictures $ (renderBlock (currentBlock g) : map renderBlock (landedBlocks g))

renderDashboard g = pictures [scorePic, nextBlockPic, landedBlockPic]
  where
    scorePic     = color white $ translate 100 50 $ scale 0.1 0.1 $ text $ "Score: " ++ (show $ score g)
    nextBlockPic = color white $ translate 100 0  $ scale 0.1 0.1 $ text $ "Rotation:" ++ (show $ rotation (currentBlock g))
    landedBlockPic = color white $ translate 100 (-50)  $ scale 0.1 0.1 $ text $ "Landed:" ++ (show $ length (landedBlocks g))
  
renderBlock :: Block -> Picture
renderBlock b = pictures $ renderTiles (tiles b) (col b)

renderTiles [] _     = []
renderTiles (t:ts) c = renderTile t c : renderTiles ts c

renderTile t c = 
  color c $ translate x y $ rectangleSolid s s
  where
    (x, y) = (pos t)
    s      = tileSize - 1

-- | (brickType g) == LineBlock   = renderLineBlock g
-- | (brickType g) == LBlock      = renderLBlock g
-- | (brickType g) == TBlock      = renderTBlock g
-- | (brickType g) == SquareBlock = renderSquareBlock g
-- | (brickType g) == SBlock      = renderSBlock g
-- | (brickType g) == ZBlock      = renderZBlock g

-- TODO: separate block definition for render code (so can do collision detection in update)
--renderLineBlock g   = color blue $ pictures [renderBlock g 0 0, renderBlock g 1 0, renderBlock g 2 0, renderBlock g 3 0]
--renderLBlock g      = color (light blue) $ pictures [renderBlock g 0 0, renderBlock g 1 0, renderBlock g 2 0, renderBlock g 2 1]
--renderTBlock g      = color (light yellow) $ pictures [renderBlock g 0 0, renderBlock g 1 0, renderBlock g 2 0, renderBlock g 1 1]
--renderSquareBlock g = color rose $ pictures [renderBlock g 0 0, renderBlock g 1 0, renderBlock g 0 1, renderBlock g 1 1]
--renderSBlock g      = color green $ pictures [renderBlock g 0 0, renderBlock g (-1) 0, renderBlock g 0 1, renderBlock g 1 1]
--renderZBlock g      = color red $ pictures [renderBlock g 0 0, renderBlock g 1 0, renderBlock g 0 1, renderBlock g (-1) 1]

handleKeys :: Event -> TetrisGame -> TetrisGame
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) g  = handleKeyPress $ g { keyPress = West }
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) g = handleKeyPress $ g { keyPress = East }
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) g  = handleKeyPress $ g { keyPress = South }
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) g    = g { currentBlock = rotateBlock (currentBlock g) }
handleKeys _ g = g { keyPress = None }

rotateBlock b =
  recreateBlock $ b { rotation = ((rotation b) + 90) `mod'` 360 }
--moveBlock g (x', y') = g
--  | y <= -290 = createNewBlock g
--  | x <= -190 || x >= 190 = g
--  | otherwise = g --{ brickPos = (x, y) }
--  where (x, y) = brickPos g + (x', y')

--createNewBlock g 
--  = g -- { brickPos = initialBrickPos, brickType = bt, brickRotation = fromIntegral br, gen = gen'' }
--  where 
--    (bt, gen')  = randomBlockType (gen g)
--    (br, gen'') = randomRotation gen'


update :: Float -> TetrisGame -> TetrisGame
update seconds g 
 = updateCurrentBlock $ handleKeyPress g

handleKeyPress g = moveBlock g (keyPress g)
updateCurrentBlock g
 | hasBlockLanded g = g { landedBlocks = (currentBlock g) : (landedBlocks g), 
                                         currentBlock = randBlock,
                                         gen = gen' }
 | otherwise = moveBlock g South
  where (randBlock, gen') = randomBlock (gen g) initialBrickPos


main = do
  is <- initGame
  play window background fps is render handleKeys update
