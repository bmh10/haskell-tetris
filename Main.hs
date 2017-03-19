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

data BlockType = LineBlock | SquareBlock | LBlock | JBlock | TBlock | SBlock | ZBlock deriving (Enum, Eq, Show, Bounded)
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

randomBlockType :: StdGen -> (BlockType, StdGen)
randomBlockType g = (toEnum $ r, g') where (r, g') = randomR (0,6) g

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
    gen :: StdGen,
    gameOver :: Bool
  } deriving Show

getLandedTiles :: TetrisGame -> [Tile]
getLandedTiles g = foldl (\ts b -> ts ++ (tiles b)) [] (landedBlocks g)

initGame = do
  stdGen <- newStdGen
  let (initialBlock, gen') = randomBlock stdGen initialBrickPos
  let initialState = Game {
      currentBlock = initialBlock,
      landedBlocks = [],
      keyPress = None,
      score = 0,
      gen = gen',
      gameOver = False
    }
  return initialState

render :: TetrisGame -> Picture 
render g = pictures [renderBlocks g, renderDashboard g]

renderBlocks :: TetrisGame -> Picture
renderBlocks g = pictures $ (renderBlock (currentBlock g) : map renderBlock (landedBlocks g))

renderDashboard g = pictures [scorePic, nextBlockPic, gameOverPic]
  where
    scorePic     = color white $ translate (-100) 275 $ scale 0.1 0.1 $ text $ "Score: " ++ (show $ score g)
    nextBlockPic = color white $ translate 50 275  $ scale 0.1 0.1 $ text $ "Next block:" ++ (show $ rotation (currentBlock g))
    gameOverPic  = color white $ translate (-100) 0  $ scale 0.3 0.3 $ text $ if (gameOver g) then "Game Over" else ""
    --landedBlockPic = color white $ translate 100 (-50)  $ scale 0.1 0.1 $ text $ "Landed:" ++ (show $ length (landedBlocks g))
    --currentBlockPosPic = color white $ translate 100 (-100)  $ scale 0.1 0.1 $ text $ "Pos:" ++ (show $ pos ((tiles (currentBlock g))!!0))
  
renderBlock :: Block -> Picture
renderBlock b = pictures $ renderTiles (tiles b) (col b)

renderTiles [] _     = []
renderTiles (t:ts) c = renderTile t c : renderTiles ts c

renderTile t c = 
  color c $ translate x y $ rectangleSolid s s
  where
    (x, y) = (pos t)
    s      = tileSize - 1

handleKeys :: Event -> TetrisGame -> TetrisGame
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) g  = handleKeyPress $ g { keyPress = West }
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) g = handleKeyPress $ g { keyPress = East }
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) g  = handleKeyPress $ g { keyPress = South }
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) g    = g { currentBlock = rotateBlock (currentBlock g) }
handleKeys _ g = g { keyPress = None }

rotateBlock b = recreateBlock $ b { rotation = ((rotation b) + 90) `mod'` 360 }

update :: Float -> TetrisGame -> TetrisGame
update seconds g = updateGameState $ updateCurrentBlock $ handleKeyPress g

handleKeyPress g = moveBlock g (keyPress g)

updateGameState g
 | isGameOver g = g { gameOver = True }
 | otherwise    = g

isGameOver g = any (\t -> snd (pos t) > y) ts
  where ts = getLandedTiles g
        y  = snd initialBrickPos

updateCurrentBlock g
 | hasBlockLanded g = checkCompletedLines
                      $ g { landedBlocks = (currentBlock g) : (landedBlocks g), 
                            currentBlock = randBlock,
                            gen = gen' }
 | otherwise = moveBlock g South
  where (randBlock, gen') = randomBlock (gen g) initialBrickPos

checkCompletedLines g = if y == 9999 then g else checkCompletedLines $ incScore 1 $ moveDown y $ clearLine y g
  where y = getFirstCompletedLine g

incScore n g = g { score = (score g) + n }

-- Move all blocks above y down 1 tile space
moveDown y g  = g { landedBlocks = moveBlocksDown (landedBlocks g) y} 
moveBlocksDown bs y = map (\b -> b { tiles = moveTilesDown (tiles b) y}) bs
moveTilesDown ts y = map (\t -> if snd (pos t) > y then t { pos = (pos t) + (0, -tileSize) } else t) ts

-- Gets  y pos of first completed line
getFirstCompletedLine g = getFirstCompletedLine' (getLandedTiles g) (snd initialBrickPos)
getFirstCompletedLine' ts y
 | y < (-300) = 9999
 | otherwise  =  if f ts y then y else getFirstCompletedLine' ts (y-tileSize) 
  where f ts y = (length $ filter (\t -> (snd (pos t)) == y) ts) == 20 

-- TODO: Gets y pos of all completed lines
--getAllCompletedLines ts = map fst $ filter (\(y,b) -> b) $ zip [(-300)..(snd initialBrickPos)] $ map (f ts) [(-300)..(snd initialBrickPos)]  
--  where f ts y = (length $ filter (\t -> (snd (pos t)) == y) ts) == 20 

-- Removes all tiles with y-pos set to y
clearLine y g  = g { landedBlocks = clearBlocks (landedBlocks g) y} 
clearBlocks bs y = map (\b -> b { tiles = clearTiles (tiles b) y}) bs
clearTiles ts y = filter (\t -> (snd (pos t)) /= y) ts

main = do
  is <- initGame
  play window background fps is render handleKeys update
