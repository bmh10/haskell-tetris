module Game where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

import Block
import Tile
import KeyPress

data Game = Game
  {
    currentBlock :: Block,
    nextBlock :: Block,
    landedBlocks :: [Block],
    keyPress :: KeyPress,
    score :: Int,
    level :: Int,
    gen :: StdGen,
    gameOver :: Bool
  } deriving Show

initialBrickPos = (10, 210)

initGame :: IO Game
initGame = do
  stdGen <- newStdGen
  let (initialBlock, gen') = randomBlock stdGen initialBrickPos
  let (nextBlock, gen'') = randomBlock gen' initialBrickPos
  let initialState = resetGame' initialBlock nextBlock gen''
  return initialState

resetGame :: Game -> Game
resetGame g = resetGame' (currentBlock g) (nextBlock g) (gen g)
resetGame' initialBlock nextBlock gen' = Game {
  currentBlock = initialBlock,
  nextBlock    = nextBlock,
  landedBlocks = [],
  keyPress = None,
  score = 0,
  level = 1,
  gen = gen',
  gameOver = False
}

-- Render functions
renderGame :: Game -> Picture 
renderGame g = pictures [renderBlocks g, renderDashboard g]

renderBlocks :: Game -> Picture
renderBlocks g = pictures $ (renderBlock (currentBlock g) : map renderBlock (landedBlocks g))

renderDashboard :: Game -> Picture
renderDashboard g = pictures [scorePic, levelPic, nextBlockText, nextBlockPic, gameOverPic, gameOverPic2]
  where
    scorePic       = color white $ translate (-150) 275 $ scale 0.1 0.1 $ text $ "Score: " ++ (show $ score g)
    levelPic       = color white $ translate (-50) 275 $ scale 0.1 0.1 $ text $ "Level: " ++ (show $ level g)
    nextBlockText  = color white $ translate 50 275  $ scale 0.1 0.1 $ text $ "Next:"
    nextBlockPic   = translate 100 170 $ scale 0.5 0.5 $ renderBlock (nextBlock g) 
    gameOverPic    = color white $ translate (-100) 0  $ scale 0.3 0.3 $ text $ if (gameOver g) then "Game Over" else ""
    gameOverPic2   = color white $ translate (-75) (-50)  $ scale 0.1 0.1 $ text $ if (gameOver g) then "Press any key to replay" else ""

-- Input handling
handleKeys :: Event -> Game -> Game
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) g  = handleKeyPress $ g { keyPress = West }
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) g = handleKeyPress $ g { keyPress = East }
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) g  = handleKeyPress $ g { keyPress = South }
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) g    = g { currentBlock = rotateBlock (currentBlock g) }
handleKeys _ g = if (gameOver g) then resetGame g else g { keyPress = None }

handleKeyPress :: Game -> Game
handleKeyPress g = moveBlock g (keyPress g)

-- Collision detection
hasBlockHitLeftSideOrAnotherBlock :: Game -> Bool
hasBlockHitLeftSideOrAnotherBlock g = hasBlockHitLeftSide (currentBlock g) || hasBlockCollided g West

hasBlockHitRightSideOrAnotherBlock :: Game -> Bool
hasBlockHitRightSideOrAnotherBlock g = hasBlockHitRightSide (currentBlock g) || hasBlockCollided g East

hasBlockLandedOrHitAnotherBlock :: Game -> Bool
hasBlockLandedOrHitAnotherBlock g = hasBlockLanded (currentBlock g) || hasBlockCollided g South

hasBlockCollided :: Game -> KeyPress -> Bool
hasBlockCollided g kp = any (hasBlockHitAnotherBlock kp (currentBlock g)) (landedBlocks g)

getLandedTiles :: Game -> [Tile]
getLandedTiles g = foldl (\ts b -> ts ++ (tiles b)) [] (landedBlocks g)

-- Update functions
updateGame :: Float -> Game -> Game
updateGame seconds g = updateGameState $ updateCurrentBlock $ handleKeyPress g

updateGameState :: Game -> Game
updateGameState g 
 | isGameOver g   = g { gameOver = True }
 | otherwise      = g

updateCurrentBlock :: Game -> Game
updateCurrentBlock g
 | (gameOver g)                      = g
 | hasBlockLandedOrHitAnotherBlock g = checkCompletedLines
					      $ g { landedBlocks = (currentBlock g) : (landedBlocks g), 
						    currentBlock = (nextBlock g),
						    nextBlock    = randBlock,
						    gen = gen' }
 | otherwise = moveBlock g South
  where (randBlock, gen') = randomBlock (gen g) initialBrickPos

isGameOver :: Game -> Bool
isGameOver g = any (\t -> snd (pos t) > y) ts
  where ts = getLandedTiles g
        y  = snd initialBrickPos

moveBlock :: Game -> KeyPress -> Game
moveBlock g kp 
 | (hasBlockLandedOrHitAnotherBlock g) || 
   (hasBlockHitLeftSideOrAnotherBlock g && kp == West) || 
   (hasBlockHitRightSideOrAnotherBlock g && kp == East) = g
 | otherwise = g { currentBlock = b { tiles = ts } }
  where 
    b = currentBlock g
    ts = moveTiles (tiles b) kp

checkCompletedLines :: Game -> Game
checkCompletedLines g = if y == 9999 then g else checkCompletedLines $ incLevel $ incScore 1 $ moveDown y $ clearLine y g
  where y = getFirstCompletedLine g
        incLevel g = if (score g) `mod` 3 == 0 then g { level = (level g) + 1 } else g

-- Gets y pos of first completed line
getFirstCompletedLine :: Game -> Float
getFirstCompletedLine g = getFirstCompletedLine' (getLandedTiles g) (snd initialBrickPos)
getFirstCompletedLine' ts y
 | y < (-300) = 9999
 | otherwise  =  if f ts y then y else getFirstCompletedLine' ts (y-tileSize) 
  where f ts y = (length $ filter (\t -> (snd (pos t)) == y) ts) == 20 

incScore :: Int -> Game -> Game
incScore n g = g { score = (score g) + n }

-- Move all blocks above y down 1 tile space
moveDown :: Float -> Game -> Game
moveDown y g  = g { landedBlocks = moveBlocksDown (landedBlocks g) y}

-- Removes all tiles with y-pos set to y
clearLine :: Float -> Game -> Game
clearLine y g  = g { landedBlocks = clearBlocks (landedBlocks g) y} 

