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
width = 420
height = 465
offset = 100
window = InWindow "Tetris" (width, height) (offset, offset)
background = black

data TetrisGame = Game
  {
    brickPos :: (Float, Float),
    gen :: StdGen
  } deriving Show

initialState :: TetrisGame
initialState = Game
  {
    brickPos = (10, 200)
  } 

render :: TetrisGame -> Picture 
render g = pictures [renderBrick g]

renderBrick g = uncurry translate (brickPos g) $ color blue $ rectangleSolid 10 10

handleKeys :: Event -> TetrisGame -> TetrisGame
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) g = g
handleKeys _ g = g

update :: Float -> TetrisGame -> TetrisGame
update seconds g = g { brickPos = (brickPos g) - (0,1) }

main = play window background fps initialState render handleKeys update
