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
width = 420
height = 465
offset = 100
window = InWindow "Tetris" (width, height) (offset, offset)
background = black

data TetrisGame = Game
  { 
    gen :: StdGen
  } deriving Show

initialState :: TetrisGame
initialState = Game {} 

render :: TetrisGame -> Picture 
render g = pictures []

handleKeys :: Event -> TetrisGame -> TetrisGame
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) g = g
handleKeys _ g = g

update :: Float -> TetrisGame -> TetrisGame
update seconds game = game

main = play window background fps initialState render handleKeys update
