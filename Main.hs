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

import Game

fps = 5
width = 400
height = 600
offset = 100

window = InWindow "Tetris" (width, height) (offset, offset)
background = black

main = do
  is <- initGame
  play window background fps is renderGame handleKeys updateGame
