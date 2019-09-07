{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module CIS194.Homework3 where
import           CodeWorld


-- Lists

data List a = Empty | Entry a (List a)

mapList :: (a -> b) -> List a -> List b
mapList _ Empty        = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

combine :: List Picture -> Picture
combine Empty        = blank
combine (Entry p ps) = p & combine ps

-- Coordinates


data Coord = C Integer Integer

data Direction = R | U | L | D

eqCoord :: Coord -> Coord -> Bool
eqCoord (C x y) (C x1 y1) = x == x1 && y == y1

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x + 1) y
adjacentCoord U (C x y) = C x (y + 1)
adjacentCoord L (C x y) = C (x - 1) y
adjacentCoord D (C x y) = C x (y - 1)

moveFromTo :: Coord -> Coord -> Coord -> Coord
moveFromTo = undefined


-- The maze

data Tile = Wall | Ground | Storage | Box | Blank deriving Eq

maze :: Coord -> Tile
maze (C x y) | abs x > 4 || abs y > 4   = Blank
             | abs x == 4 || abs y == 4 = Wall
             | x == 2 && y <= 0         = Wall
             | x == 3 && y <= 0         = Storage
             | x >= -2 && y == 0        = Box
             | otherwise                = Ground

noBoxMaze :: Coord -> Tile
noBoxMaze c = case maze c of
  Box -> Ground
  t   -> t

mazeWithBoxes :: List Coord -> Coord -> Tile
mazeWithBoxes Empty c = noBoxMaze c
mazeWithBoxes (Entry b bs) c | eqCoord b c = Box
                             | otherwise   = mazeWithBoxes bs c

allCoords :: [Coord]
allCoords = [ (C i j) | i <- [-10 .. 10], j <- [-10 .. 10] ]

-- The state

data PlayerState = PlayerState Coord Direction

playerInitState :: PlayerState
playerInitState = PlayerState (head freeGround) R
  where freeGround = filter (\c -> maze c == Ground) allCoords


data State = State PlayerState BoxesState
-- FIXME!


type BoxesState = List Coord

initialBoxes :: List Coord
initialBoxes = foldl
  (\acc c -> (Entry c acc))
  Empty
  [ (C i j) | i <- [-10 .. 10], j <- [-10 .. 10], isBox (C i j) ]
 where
  isBox c = case maze c of
    Box -> True
    _   -> False


initialState :: State
initialState = State playerInitState initialBoxes

-- Event handling

movePlayer :: Direction -> State -> State
movePlayer d s@(State (PlayerState c _) boxes)
  | t1 == Ground || t1 == Storage
  = (State (PlayerState (adjacentCoord d c) d) boxes)
  | t1 == Box && (t2 == Ground || t2 == Storage)
  = (State (PlayerState (adjacentCoord d c) d) moveBox)
  | otherwise
  = s
 where
  [_, c1, c2] = take 3 $ (iterate (adjacentCoord d) c)
  [t1, t2]    = map (mazeWithBoxes boxes) [c1, c2]
  moveBox     = mapList (\bc -> if eqCoord bc c1 then c2 else bc) boxes

isWon :: State -> Bool
isWon (State _ boxes) = allList $ mapList isOnStorage boxes
 where
  isOnStorage c = maze c == Storage
  allList Empty        = True
  allList (Entry x xs) = x && allList (xs)

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) s | key == "Right" = movePlayer R s
                             | key == "Up"    = movePlayer U s
                             | key == "Left"  = movePlayer L s
                             | key == "Down"  = movePlayer D s
                             | otherwise      = s
handleEvent _ s = s
-- Drawing

wall, ground, storage, box :: Picture
wall = colored grey (solidRectangle 1 1)
ground = colored yellow (solidRectangle 1 1)
storage = colored white (solidCircle 0.3) & ground
box = colored brown (solidRectangle 1 1)

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

pictureOfMaze :: Picture
pictureOfMaze = draw21times (\r -> draw21times (\c -> drawTileAt (C r c)))

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
 where
  go :: Integer -> Picture
  go 11 = blank
  go n  = something n & go (n + 1)

drawTileAt :: Coord -> Picture
drawTileAt c = atCoord c (drawTile (noBoxMaze c))


atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic


player :: Direction -> Picture
player R =
  translated 0 0.3 cranium
    & polyline [(0, 0), (0.3, 0.05)]
    & polyline [(0, 0), (0.3, -0.05)]
    & polyline [(0, -0.2), (0, 0.1)]
    & polyline [(0, -0.2), (0.1, -0.5)]
    & polyline [(0, -0.2), (-0.1, -0.5)]
  where cranium = circle 0.18 & sector (7 / 6 * pi) (1 / 6 * pi) 0.18
player L = scaled (-1) 1 (player R) -- Cunning!
player U =
  translated 0 0.3 cranium
    & polyline [(0, 0), (0.3, 0.05)]
    & polyline [(0, 0), (-0.3, 0.05)]
    & polyline [(0, -0.2), (0, 0.1)]
    & polyline [(0, -0.2), (0.1, -0.5)]
    & polyline [(0, -0.2), (-0.1, -0.5)]
  where cranium = solidCircle 0.18
player D =
  translated 0 0.3 cranium
    & polyline [(0, 0), (0.3, -0.05)]
    & polyline [(0, 0), (-0.3, -0.05)]
    & polyline [(0, -0.2), (0, 0.1)]
    & polyline [(0, -0.2), (0.1, -0.5)]
    & polyline [(0, -0.2), (-0.1, -0.5)]
 where
  cranium = circle 0.18 & translated 0.06 0.08 (solidCircle 0.04) & translated
    (-0.06)
    0.08
    (solidCircle 0.04)

pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes cs = combine (mapList (\c -> atCoord c (drawTile Box)) cs)

pictureOfPlayer :: PlayerState -> Picture
pictureOfPlayer (PlayerState c d) = atCoord c (player d)

drawState :: State -> Picture
-- drawState State = pictureOfMaze
drawState (State p boxes) =
  (pictureOfPlayer p) & (pictureOfBoxes boxes) & pictureOfMaze

-- The complete interaction

sokoban :: Interaction State
sokoban = Interaction initialState (\_ c -> c) handleEvent drawState

-- The general interaction type

data Interaction world = Interaction
        world
        (Double -> world -> world)
        (Event -> world -> world)
        (world -> Picture)


runInteraction :: Interaction s -> IO ()
runInteraction (Interaction state0 step handle draw) =
  interactionOf state0 step handle draw

-- End Game
-- data EndGame s = EndGame | StillRunning s

endGame :: Interaction State -> Interaction State
endGame (Interaction state0 step handle draw) = Interaction state0
                                                            step
                                                            handle'
                                                            draw'
 where
  handle' e s | isWon s   = s
              | otherwise = handle e s
  draw' s | isWon s   = scaled 3 3 (lettering "You Won")
          | otherwise = draw s



-- Resetable interactions

resetable :: Interaction s -> Interaction s
resetable (Interaction state0 step handle draw) = Interaction state0
                                                              step
                                                              handle'
                                                              draw
 where
  handle' (KeyPress key) _ | key == "Esc" = state0
  handle' e s                             = handle e s

-- Start screen

startScreen :: Picture
startScreen = scaled 3 3 (lettering "Sokoban!")

data SSState world = StartScreen | Running world

withStartScreen :: Interaction s -> Interaction (SSState s)
withStartScreen (Interaction state0 step handle draw) = Interaction state0'
                                                                    step'
                                                                    handle'
                                                                    draw'
 where
  state0' = StartScreen

  step' _ StartScreen = StartScreen
  step' t (Running s) = Running (step t s)

  handle' (KeyPress key) StartScreen | key == " " = Running state0
  handle' _ StartScreen                           = StartScreen
  handle' e (Running s)                           = Running (handle e s)

  draw' StartScreen = startScreen
  draw' (Running s) = draw s


-- The main function

main :: IO ()
main = runInteraction $ resetable $ withStartScreen $ endGame sokoban
-- main = runInteraction sokoban


