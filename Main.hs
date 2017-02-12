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

fps = 50
tileSize = 20
width = 420
height = 465
offset = 100
window = InWindow "Tetris" (width, height) (offset, offset)
background = black

data BlockType = LineBlock | LBlock | TBlock deriving (Eq, Show)

data TetrisGame = Game
  {
    brickPos :: (Float, Float),
    brickType :: BlockType,
    gen :: StdGen
  } deriving Show

initialState :: TetrisGame
initialState = Game
  {
    brickPos = (10, 200),
    brickType = TBlock
  } 

render :: TetrisGame -> Picture 
render g = pictures [renderBrick g]

renderBrick g
 | (brickType g) == LineBlock = renderLineBlock g
 | (brickType g) == LBlock    = renderLBlock g
 | (brickType g) == TBlock    = renderTBlock g

renderLineBlock g = color blue $ pictures [renderBlock g 0 0, renderBlock g 1 0, renderBlock g 2 0, renderBlock g 3 0]
renderLBlock g    = color (light blue) $ pictures [renderBlock g 0 0, renderBlock g 1 0, renderBlock g 2 0, renderBlock g 2 1]
renderTBlock g    = color (light yellow) $ pictures [renderBlock g 0 0, renderBlock g 1 0, renderBlock g 2 0, renderBlock g 1 1]

renderBlock g ox oy = 
  translate x' y' $ rectangleSolid (tileSize-1) (tileSize-1)
  where
    (x, y) = brickPos g
    x' = x + ox*tileSize
    y' = y + oy*tileSize

handleKeys :: Event -> TetrisGame -> TetrisGame
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) g = g
handleKeys _ g = g

update :: Float -> TetrisGame -> TetrisGame
update seconds g = g { brickPos = (brickPos g) - (0,1) }

main = play window background fps initialState render handleKeys update
