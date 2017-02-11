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
{-
 TODO: 
   1. Refactor code
-}
fps = 5
width = 420 -- 28 * 15
height = 465 + dashboardHeight -- 31 * 15
dashboardHeight = 20
offset = 100
tileSize = 15
maxTileHoriz = 27
pacmanInitialPos = (13,23)
redGhostInitialPos = (13,11)
blueGhostInitialPos = (15,15)
centerPos = (14,14)
aboveCenterPos = (14,11)
yellowGhostInitialPos = (11,15)
pinkGhostInitialPos = (13,14)
pacmanInitialLives = 3
pacmanInitialDir = East
ghostInitialDir = East
window = InWindow "Pacman" (width, height) (offset, offset)
background = black

data Direction = North | East | South | West | None deriving (Enum, Eq, Show, Bounded)
data PlayerState = Normal | CenterZone | Scared | Returning deriving (Eq, Show)
data GameState = Playing | Won | Lost deriving (Eq, Show) 

oppositeDir :: Direction -> Direction
oppositeDir North = South
oppositeDir South = North
oppositeDir East  = West
oppositeDir West  = East
oppositeDir None  = None

randomDir :: StdGen -> (Direction, StdGen)
randomDir g = (toEnum $ r, g') where (r, g') = randomR (0,3) g

data PacmanGame = Game
  { 
    level :: [String],           -- Updated level layout
    initialLevel :: [String],    -- Initial level layout
    pacmanPos :: (Int, Int),     -- Tile coord of pacman
    pacmanDir :: Direction,      -- Pacman's direction of travel
    pacmanNextDir :: Direction,  -- Buffered next direction
    ghostPos :: [(Int, Int)],    -- Position of all ghosts
    ghostDir :: [Direction],     -- Direction of all ghosts
    ghostState :: [PlayerState], -- State of all ghosts
    score :: Int,                -- Current score 
    lives :: Int,                -- Current lives
    seconds :: Float,            -- Game timer
    gen :: StdGen,               -- Random number generator
    scaredTimer :: Int,          -- Scared ghost timer
    paused :: Bool,              -- Paused or not
    countdownTimer :: Int,       -- Start of game timer
    gameState :: GameState,      -- State of the game
    coinCount :: Int,            -- Number of remaining coins
    ghostEatenCount :: Int       -- Numbers of ghosts eaten
  } deriving Show 

-- Tile functions
getTile :: Int -> Int -> PacmanGame -> Char
getTile x y g = (level g) !! y !! x

setTile :: Int -> Int -> Char -> PacmanGame -> PacmanGame
setTile x y c g = g {level = updatedLevel}
  where updatedLevel = setAtIdx y (setAtIdx x c ((level g) !! y)) (level g)

countCoins :: [String] -> Int
countCoins level = foldl (\acc x -> acc + length x) 0 $ map (filter (=='.')) level

onTick :: PacmanGame -> Bool -> Int -> a -> a -> a 
onTick g c t a b = if (c && (mod (round (seconds g)) t) == 0) then a else b

-- Map tile coords ((0,0) is top-left tile) to actual screen coords ((0, 0) is center of screen)
tileToCoord :: (Int, Int) -> (Float, Float) 
tileToCoord (x, y) = (fromIntegral x*tileSize + tileSize/2 - fromIntegral width/2, fromIntegral height/2 - fromIntegral y*tileSize - tileSize/2)

setAtIdx :: Int -> a -> [a] -> [a]
setAtIdx idx val xs = take idx xs ++ [val] ++ drop (idx+1) xs

-- Rendering
render :: PacmanGame -> Picture 
render g = pictures [renderLevel g, 
                     renderPlayer "pacman" (pacmanPos g) (pacmanDir g) Normal g,
                     renderPlayer "redGhost" ((ghostPos g) !! 0) ((ghostDir g) !! 0) ((ghostState g) !! 0) g,
                     renderPlayer "blueGhost" ((ghostPos g) !! 1) ((ghostDir g) !! 1) ((ghostState g) !! 1) g,
                     renderPlayer "yellowGhost" ((ghostPos g) !! 2) ((ghostDir g) !! 2) ((ghostState g) !! 2) g,
                     renderPlayer "pinkGhost" ((ghostPos g) !! 3) ((ghostDir g) !! 3) ((ghostState g) !! 3) g,
                     renderDashboard g,
                     renderMessage g]

renderPlayer :: String -> (Int, Int) -> Direction -> PlayerState -> PacmanGame -> Picture 
renderPlayer player (x, y) dir state game = translate x' y' $ color blue $ circleSolid 2
  where 
    (x', y') = tileToCoord (x, y)
    file = getFile player dir state game

-- TODO: should preload images
getFile :: String -> Direction -> PlayerState -> PacmanGame -> String
getFile player dir state game
 | state == Scared = "img/scaredGhost" ++ step ++ ".png"
 | state == Returning = "img/eyes.png"
 | otherwise = "img/" ++ player ++ show dir ++ step ++ ".png"
  where 
    step = onTick game True 2 "1" "2"

renderDashboard :: PacmanGame -> Picture
renderDashboard g = pictures $ [scorePic, livesTxt] ++ livesPic
  where
    scorePic = color white $ translate (-80) (-fromIntegral height/2 + 5) $ scale 0.1 0.1 $ text $ "Score: " ++ (show $ score g)
    livesTxt = color white $ translate 20 (-fromIntegral height/2 + 5) $ scale 0.1 0.1 $ text "Lives:"
    livesPic = genLivesPic (lives g)

    genLivesPic :: Int -> [Picture]
    genLivesPic 0 = [blank]
    genLivesPic n = (translate (50 + fromIntegral n*tileSize) (-fromIntegral height/2 + 10) $ circleSolid 2) : genLivesPic (n-1)

renderMessage :: PacmanGame -> Picture
renderMessage g = pictures [countdownPic, statusMsg]
  where 
    countdownPic = if (countdownTimer g) > 0 then color white $ translate 0 10 $ scale 0.3 0.3 $ text $ show $ countdownTimer g else blank
    statusMsg    = if (gameState g) /= Playing then color white $ translate (-100) 10 $ scale 0.3 0.3 $ text msg else blank
    msg          = if (gameState g) == Won then "You won" else "Game Over"

renderLevel :: PacmanGame -> Picture
renderLevel game = renderLines (level game) 0

renderLines :: [String] -> Int -> Picture
renderLines [] _ = blank
renderLines (l:ls) y = pictures [renderLine l 0 y, renderLines ls (y+1)]

renderLine :: String -> Int -> Int -> Picture
renderLine [] _ _      = blank
renderLine (t:ts) x y  = pictures [renderTile t x y, renderLine ts (x+1) y]

renderTile :: Char -> Int -> Int -> Picture
renderTile c x y
 | c == 'x'  = translate x' y' $ color blue $ rectangleSolid (tileSize-1) (tileSize-1)
 | c == '+'  = translate x' y' $ color white $ rectangleSolid (tileSize-1) 2
 | c == '.'  = translate x' y' $ color yellow $ circleSolid 2
 | c == 'o'  = translate x' y' $ color yellow $ circleSolid 4
 | otherwise = blank
  where
    (x', y') = tileToCoord (x, y)

-- Event handling
handleKeys :: Event -> PacmanGame -> PacmanGame
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) g  = setPacmanDir East g
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) g   = setPacmanDir West g
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) g     = setPacmanDir North g
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) g   = setPacmanDir South g
handleKeys (EventKey (Char 'p') Down _ _) g = g {paused = not (paused g)}
handleKeys _ game
 | (gameState game) /= Playing = resetGameFully game
 | otherwise = game

setPacmanDir dir g
 | (pacmanDir g) == oppositeDir dir = g { pacmanDir = dir, pacmanNextDir = None } 
 | otherwise                        = g { pacmanNextDir = dir }

-- Have to update lives twice to prevent missed collision
update :: Float -> PacmanGame -> PacmanGame
update secs game
 | (paused game)               = game
 | (gameState game) /= Playing = game
 | (countdownTimer game) > 0   = onTick game True 4 (decrementCountdown $ updateSeconds game) (updateSeconds $ game)
 | otherwise                   = updateScore $ updateLives $ updateGhosts 3 $ updateLives $ updatePacman $ updateSeconds game

updateSeconds :: PacmanGame -> PacmanGame
updateSeconds game = game {seconds = (seconds game) + 1, scaredTimer = (scaredTimer game) + 1}

decrementCountdown :: PacmanGame -> PacmanGame
decrementCountdown game = game {countdownTimer = (countdownTimer game) - 1}

-- TODO: separate ghost state update  from lives update
updateLives :: PacmanGame -> PacmanGame
updateLives g
 | ghostIdx == Nothing = g
 | state == Returning  = g
 | state == Scared     = setGhostReturning g (fromJust ghostIdx) 
 | (lives g) > 1       = resetGame $ g {lives = (lives g) - 1}
 | otherwise           = g {lives = 0, gameState = Lost}
  where
    ghostIdx = elemIndex (pacmanPos g) (ghostPos g)
    state    = (ghostState g) !! (fromJust ghostIdx)

updateScore :: PacmanGame -> PacmanGame
updateScore g
  | coinCount g == 0 = g {gameState = Won}
  | tile == '.'      = setBlankTile $ g { score = s + 10, coinCount = (coinCount g) - 1 }
  | tile == 'o'      = setGhostsScared $ setBlankTile $ g { score = s + 50, ghostEatenCount = 0 }
  | otherwise        = g
  where
    (x, y) = pacmanPos g
    s = score g
    tile = getTile x y g
    setBlankTile = setTile x y '_'

setGhostsScared g = g {ghostState = replicate 4 Scared, scaredTimer = 0}
setGhostReturning g idx = g {ghostState = setAtIdx idx Returning (ghostState g), ghostEatenCount = newEatenCount, score = newScore}
  where
    newEatenCount = (ghostEatenCount g) + 1
    newScore      = (score g) + calcBonus newEatenCount
    calcBonus 1 = 200
    calcBonus n = calcBonus (n-1) * 2

updatePacman g = updatePacmanPos g

-- If ghost does not move after update (e.g. hit a wall), change direction then update again
updateGhosts :: Int -> PacmanGame -> PacmanGame
updateGhosts 0 g = updateGhost 0 g
updateGhosts n g = updateGhost n $ updateGhosts (n-1) g

updateGhost :: Int -> PacmanGame -> PacmanGame
updateGhost idx g = updateGhostState idx $ updateGhostPos idx $ updateGhostDir idx g

updateGhostState idx g
 | state == Scared && (scaredTimer g) > 50         = g {ghostState = setAtIdx idx Normal (ghostState g)}
 | state == Returning && (x, y) == centerPos       = g {ghostState = setAtIdx idx CenterZone (ghostState g)}
 | state == CenterZone && getTile x (y+1) g == '+' = g {ghostState = setAtIdx idx Normal (ghostState g)}
 | otherwise = g
  where
    (x, y) = (ghostPos g) !! idx
    state = (ghostState g) !! idx

updateGhostDir idx g
 | atJunction (x, y) dir state g  = updateRandGen sg $ g {ghostDir = setAtIdx idx dir' (ghostDir g)}
 | otherwise = g
  where
    (x, y)   = (ghostPos g) !! idx
    dir      = (ghostDir g) !! idx
    state    = (ghostState g) !! idx
    (x', y') = move (x, y) dir
    (dir', sg) = (calculateGhostNextDir idx g)
    updateRandGen :: (Maybe StdGen) -> PacmanGame -> PacmanGame
    updateRandGen Nothing g = g
    updateRandGen (Just sg) g = g {gen = sg}

updateGhostPos idx g
  | canMove state (x, y) dir g = g {ghostPos = setAtIdx idx (x', y') (ghostPos g)}
  | otherwise = g
  where
    (x, y)   = (ghostPos g) !! idx
    dir      = (ghostDir g) !! idx
    state    = (ghostState g) !! idx
    (x', y') = onTick g (state == Scared) 2 (x, y) (move (x, y) dir)

atJunction (x, y) dir st g = 
  ((dir == North || dir == South) && (canMove st (x, y) East g || canMove st (x, y) West g)) ||
  ((dir == East || dir == West) && (canMove st (x, y) North g || canMove st (x, y) South g))

calculateGhostNextDir :: Int -> PacmanGame -> (Direction, Maybe StdGen)
calculateGhostNextDir idx g
 | state == CenterZone = calculateNextDir g state dir pos aboveCenterPos
 | state == Returning = calculateNextDir g state dir pos centerPos
 | state == Normal = calculateNextDir g state dir pos (getTileToTarget idx g)
 | otherwise = (randDir, Just g')
  where 
    pos   = (ghostPos g) !! idx
    dir      = (ghostDir g) !! idx
    state    = (ghostState g) !! idx
    (randDir, g') = randomDir (gen g)

getTileToTarget idx g
 | idx == 0 = (x, y)
 | idx == 1 = (x-1, y-1)
 | idx == 2 = (x+1, y+1)
 | idx == 3 = (x+10, y-10)
 | otherwise = (x, y)
   where
     (x, y) = pacmanPos g

calculateNextDir :: PacmanGame -> PlayerState -> Direction -> (Int,Int) -> (Int,Int) -> (Direction, Maybe StdGen)
calculateNextDir g st curDir (x,y) (tx,ty)
 | x < tx && canMove st (x,y) East g && curDir /= West = (East, Nothing)
 | x > tx && canMove st (x,y) West g && curDir /= East = (West, Nothing)
 | y < ty && canMove st (x,y) South g && curDir /= North = (South, Nothing)
 | y > ty && canMove st (x,y) North g && curDir /= South = (North, Nothing)
 | mx >= my && canMove st (x,y) South g && curDir /= North = (South, Nothing)
 | mx >= my && canMove st (x,y) North g && curDir /= South = (North, Nothing)
 | my >= mx && canMove st (x,y) East g && curDir /= West = (East, Nothing)
 | my >= mx && canMove st (x,y) West g && curDir /= East = (West, Nothing)
 | otherwise = (randDir, Just g')
  where
    mx = abs $ x - tx
    my = abs $ y - ty
    (randDir, g') = randomDir (gen g)

updatePacmanPos :: PacmanGame -> PacmanGame
updatePacmanPos g
 | canMove Normal (x, y) nextDir g = g { pacmanPos = (move (x, y) nextDir), pacmanDir = nextDir, pacmanNextDir = None }
 | canMove Normal (x, y) dir g     = g { pacmanPos = (move (x, y) dir) }
 | otherwise                = g
  where
    dir = pacmanDir g
    nextDir = pacmanNextDir g
    (x, y) = pacmanPos g

move :: (Int, Int) -> Direction -> (Int, Int)
move (x, y) None = (x, y)
move (x, y) East = (wrapx $ x+1, y)
move (x, y) West = (wrapx $ x-1, y)
move (x, y) North = (x, y-1)
move (x, y) South = (x, y+1)

canMove :: PlayerState -> (Int, Int) -> Direction -> PacmanGame -> Bool
canMove _ _ None _ = False
canMove state (x, y) dir g = canMoveTo g state dir $ move (x, y) dir

canMoveTo :: PacmanGame -> PlayerState -> Direction -> (Int, Int) -> Bool
canMoveTo g state dir (x, y) = getTile x y g /= 'x' && not (getTile x y g == '+' && dir == South && state /= Returning)

wrapx x
 | x < 0 = maxTileHoriz
 | x > maxTileHoriz = 0
 | otherwise = x

resetGame g = g { pacmanPos = pacmanInitialPos, pacmanDir = pacmanInitialDir, ghostPos = [redGhostInitialPos, blueGhostInitialPos, yellowGhostInitialPos, pinkGhostInitialPos], ghostDir = replicate 4 ghostInitialDir, ghostState = replicate 4 CenterZone, seconds = 0, pacmanNextDir = None, scaredTimer = 0, countdownTimer = 3}

resetGameFully g = resetGame $ g {gameState = Playing, lives = pacmanInitialLives, score = 0, level = (initialLevel g), coinCount = countCoins (initialLevel g)}

-- Not sure why print is required...
initTiles = do 
  contents <- readFile "pacman.lvl"
  stdGen <- newStdGen
  let rows = words contents
  let initialState = Game { level = rows, initialLevel = rows, pacmanPos = pacmanInitialPos, pacmanDir = pacmanInitialDir, ghostPos = [redGhostInitialPos, blueGhostInitialPos, yellowGhostInitialPos, pinkGhostInitialPos], ghostDir = replicate 4 ghostInitialDir, ghostState = replicate 4 CenterZone, score = 0, seconds = 0, lives = pacmanInitialLives, pacmanNextDir = None, gen = stdGen, scaredTimer = 0, paused = False, countdownTimer = 3, gameState = Playing, coinCount = countCoins rows, ghostEatenCount = 0 }
  print rows
  return initialState

main = do
  initialState <- initTiles
  play window background fps initialState render handleKeys update
