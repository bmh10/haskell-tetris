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
initialBrickPos = (10, 200)
window = InWindow "Tetris" (width, height) (offset, offset)
background = black

data BlockType = LineBlock | LBlock | TBlock | SBlock | ZBlock | SquareBlock deriving (Enum, Eq, Show, Bounded)
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

createBlock LineBlock r (x,y) = Block {
  blockType = LineBlock,
  col       = blue,
  rotation  = r,
  tiles     = createBlockLine (x,y) 4
}

moveBlock g kp = moveTiles (tiles (currentBlock g)) kp

moveTiles ts kp = map (moveTile kp) ts

moveTile kp t = t { pos = (x, y) }
  where (x,y) = (pos t) + (func kp)

func kp 
 | kp == South = (0, -tileSize)
 | kp == East  = (tileSize, 0)
 | kp == West  = (-tileSize, 0)
 | kp == None  = (0, -tileSize)

createBlockLine _ 0 = []
createBlockLine (x,y) l = createTile x y : createBlockLine (x+tileSize,y) (l-1)

data TetrisGame = Game
  {
    currentBlock :: Block,
    --brickPos :: (Float, Float),
    --brickRotation :: Float,
    --brickType :: BlockType,
    keyPress :: KeyPress,
    score :: Int,
    gen :: StdGen
  } deriving Show

initGame = do
  stdGen <- newStdGen
  let initialBlock = createBlock LineBlock 0 (10, 10)
  let initialState = createNewBlock $ Game {
      currentBlock = initialBlock,
      --brickPos = initialBrickPos,
      --brickRotation = 0,
      --brickType = LineBlock,
      keyPress = None,
      score = 0,
      gen = stdGen
    }
  return initialState

randomBlockType :: StdGen -> (BlockType, StdGen)
randomBlockType g = (toEnum $ r, g') where (r, g') = randomR (0,3) g

randomRotation :: StdGen -> (Int, StdGen)
randomRotation g = (90*r, g') where (r, g') = randomR (0,3) g

render :: TetrisGame -> Picture 
--render g = pictures [translate x y $ rotate (brickRotation g) $ translate (-x) (-y) $ renderBrick g, renderDashboard g]
render g = pictures $ renderBlock g  -- renderDashboard g
  --where (x, y) = brickPos g

renderDashboard g = pictures [scorePic, nextBlockPic]
  where
    scorePic     = color white $ translate 100 50 $ scale 0.1 0.1 $ text $ "Score: " ++ (show $ score g)
    nextBlockPic = color white $ translate 100 0  $ scale 0.1 0.1 $ text $ "Next block:"
  

renderBlock g
 = renderTiles (tiles curBlock) (col curBlock)
  where curBlock = (currentBlock g)

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

--renderBlock g ox oy = 
--  translate x' y' $ rectangleSolid (tileSize-1) (tileSize-1)
--  where
--    (x, y) = brickPos g
--    x' = x + ox*tileSize
--    y' = y + oy*tileSize

handleKeys :: Event -> TetrisGame -> TetrisGame
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) g  = handleKeyPress $ g { keyPress = West }
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) g = handleKeyPress $ g { keyPress = East }
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) g  = handleKeyPress $ g { keyPress = South }
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) g    = g --{ brickRotation = ((brickRotation g) + 90) `mod'` 360 }
handleKeys _ g = g { keyPress = None }

--moveBlock g (x', y') = g
--  | y <= -290 = createNewBlock g
--  | x <= -190 || x >= 190 = g
--  | otherwise = g --{ brickPos = (x, y) }
--  where (x, y) = brickPos g + (x', y')

createNewBlock g 
  = g -- { brickPos = initialBrickPos, brickType = bt, brickRotation = fromIntegral br, gen = gen'' }
  where 
    (bt, gen')  = randomBlockType (gen g)
    (br, gen'') = randomRotation gen'


handleKeyPress g = moveBlock g (keyPress g)

updateBlock g = moveBlock g (0, -tileSize)

update :: Float -> TetrisGame -> TetrisGame
update seconds g 
 = handleKeyPress $ updateBlock g

main = do
  is <- initGame
  play window background fps is render handleKeys update
