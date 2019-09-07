{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module CIS194.Homework4 where
import           CodeWorld

-- (+ 1 2 3)
-- (+) 1 2
-- foldl (+) 0 [1..4]
-- sum [1..4]
-- product [1..4]


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
  handle' e s              = handle e s

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
  handle' _ StartScreen              = StartScreen
  handle' e (Running s)              = Running (handle e s)

  draw' StartScreen = startScreen
  draw' (Running s) = draw s


-- The main function

main :: IO ()
main = runInteraction $ resetable $ withStartScreen $ endGame sokoban
-- main = runInteraction sokoban


data Maze = Maze Coord (Coord -> Tile)

mazes :: List Maze
mazes =
  Entry (Maze (C 1 1) maze9)
    $ Entry (Maze (C 0 0) maze8)
    $ Entry (Maze (C (-3) 3) maze7)
    $ Entry (Maze (C (-2) 4) maze6)
    $ Entry (Maze (C 0 1) maze5)
    $ Entry (Maze (C 1 (-3)) maze4)
    $ Entry (Maze (C (-4) 3) maze3)
    $ Entry (Maze (C 0 1) maze1)
    $ Empty

extraMazes :: List Maze
extraMazes =
  Entry (Maze (C 1 (-3)) maze4')
    $ Entry (Maze (C 1 (-3)) maze4'')
    $ Entry (Maze (C 1 1) maze9')
    $ mazes

maze1 :: Coord -> Tile
maze1 (C x y) | abs x > 4 || abs y > 4   = Blank
              | abs x == 4 || abs y == 4 = Wall
              | x == 2 && y <= 0         = Wall
              | x == 3 && y <= 0         = Storage
              | x >= -2 && y == 0        = Box
              | otherwise                = Ground

maze3 :: Coord -> Tile
maze3 (C (-5) (-5)) = Wall
maze3 (C (-5) (-4)) = Wall
maze3 (C (-5) (-3)) = Wall
maze3 (C (-5) (-2)) = Wall
maze3 (C (-5) (-1)) = Wall
maze3 (C (-5) 0   ) = Wall
maze3 (C (-5) 1   ) = Wall
maze3 (C (-5) 2   ) = Wall
maze3 (C (-5) 3   ) = Wall
maze3 (C (-5) 4   ) = Wall

maze3 (C (-4) (-5)) = Wall
maze3 (C (-4) (-4)) = Ground
maze3 (C (-4) (-3)) = Ground
maze3 (C (-4) (-2)) = Ground
maze3 (C (-4) (-1)) = Ground
maze3 (C (-4) 0   ) = Ground
maze3 (C (-4) 1   ) = Ground
maze3 (C (-4) 2   ) = Ground
maze3 (C (-4) 3   ) = Ground
maze3 (C (-4) 4   ) = Wall

maze3 (C (-3) (-5)) = Wall
maze3 (C (-3) (-4)) = Ground
maze3 (C (-3) (-3)) = Wall
maze3 (C (-3) (-2)) = Wall
maze3 (C (-3) (-1)) = Wall
maze3 (C (-3) 0   ) = Wall
maze3 (C (-3) 1   ) = Ground
maze3 (C (-3) 2   ) = Wall
maze3 (C (-3) 3   ) = Ground
maze3 (C (-3) 4   ) = Wall
maze3 (C (-3) 5   ) = Wall

maze3 (C (-2) (-5)) = Wall
maze3 (C (-2) (-4)) = Box
maze3 (C (-2) (-3)) = Ground
maze3 (C (-2) (-2)) = Ground
maze3 (C (-2) (-1)) = Ground
maze3 (C (-2) 0   ) = Wall
maze3 (C (-2) 1   ) = Ground
maze3 (C (-2) 2   ) = Box
maze3 (C (-2) 3   ) = Box
maze3 (C (-2) 4   ) = Ground
maze3 (C (-2) 5   ) = Wall

maze3 (C (-1) (-6)) = Wall
maze3 (C (-1) (-5)) = Wall
maze3 (C (-1) (-4)) = Ground
maze3 (C (-1) (-3)) = Ground
maze3 (C (-1) (-2)) = Ground
maze3 (C (-1) (-1)) = Ground
maze3 (C (-1) 0   ) = Wall
maze3 (C (-1) 1   ) = Ground
maze3 (C (-1) 2   ) = Ground
maze3 (C (-1) 3   ) = Box
maze3 (C (-1) 4   ) = Ground
maze3 (C (-1) 5   ) = Wall
maze3 (C (-1) 6   ) = Wall

maze3 (C 0    (-6)) = Wall
maze3 (C 0    (-5)) = Ground
maze3 (C 0    (-4)) = Ground
maze3 (C 0    (-3)) = Ground
maze3 (C 0    (-2)) = Ground
maze3 (C 0    (-1)) = Ground
maze3 (C 0    0   ) = Wall
maze3 (C 0    1   ) = Wall
maze3 (C 0    2   ) = Wall
maze3 (C 0    3   ) = Wall
maze3 (C 0    4   ) = Ground
maze3 (C 0    5   ) = Ground
maze3 (C 0    6   ) = Wall

maze3 (C 1    (-6)) = Wall
maze3 (C 1    (-5)) = Ground
maze3 (C 1    (-4)) = Ground
maze3 (C 1    (-3)) = Ground
maze3 (C 1    (-2)) = Ground
maze3 (C 1    (-1)) = Ground
maze3 (C 1    0   ) = Wall
maze3 (C 1    1   ) = Storage
maze3 (C 1    2   ) = Storage
maze3 (C 1    3   ) = Storage
maze3 (C 1    4   ) = Ground
maze3 (C 1    5   ) = Ground
maze3 (C 1    6   ) = Wall

maze3 (C 2    (-6)) = Wall
maze3 (C 2    (-5)) = Wall
maze3 (C 2    (-4)) = Ground
maze3 (C 2    (-3)) = Ground
maze3 (C 2    (-2)) = Ground
maze3 (C 2    (-1)) = Ground
maze3 (C 2    0   ) = Wall
maze3 (C 2    1   ) = Wall
maze3 (C 2    2   ) = Wall
maze3 (C 2    3   ) = Wall
maze3 (C 2    4   ) = Wall
maze3 (C 2    5   ) = Wall
maze3 (C 2    6   ) = Wall

maze3 (C 3    (-5)) = Wall
maze3 (C 3    (-4)) = Ground
maze3 (C 3    (-3)) = Ground
maze3 (C 3    (-2)) = Storage
maze3 (C 3    (-1)) = Ground
maze3 (C 3    0   ) = Wall

maze3 (C 4    (-5)) = Wall
maze3 (C 4    (-4)) = Wall
maze3 (C 4    (-3)) = Wall
maze3 (C 4    (-2)) = Wall
maze3 (C 4    (-1)) = Wall
maze3 (C 4    0   ) = Wall

maze3 _             = Blank

maze4 :: Coord -> Tile
maze4 (C x y) | abs x > 4 || abs y > 4      = Blank
              | abs x == 4 || abs y == 4    = Wall
              | x == 2 && y < 0             = Wall
              | x >= -1 && y == 1 && x <= 2 = Wall
              | x == -3 && y == 1           = Wall
              | x == 0 && y == 3            = Wall
              | x == 0 && y == 0            = Wall
              | x == 3 && y == -3           = Storage
              | x == 1 && y == 2            = Storage
              | x == -3 && y == 2           = Storage
              | x == 1 && y == -1           = Storage
              | x == -2 && y == 1           = Box
              | x == 2 && y == 2            = Box
              | x <= 1 && y == -2 && x >= 0 = Box
              | otherwise                   = Ground

maze5 :: Coord -> Tile
maze5 (C x y) | abs x > 4 || abs y > 4     = Blank
              | abs x == 4 || abs y == 4   = Wall
              | x == 1 && y < 0            = Wall
              | x == -3 && y == -2         = Wall
              | x <= 1 && x > -2 && y == 0 = Wall
              | x > -3 && x < 3 && y == 2  = Wall
              | x == 3 && y > 1            = Storage
              | y == -2 && x < 0           = Box
              | y == -2 && x == 2          = Box
              | y == 0 && x == 3           = Box
              | y == -1 && x > 1 && x < 4  = Storage
              | otherwise                  = Ground

maze6 :: Coord -> Tile
maze6 (C x y) | abs x > 3 || abs y > 5                  = Blank
              | abs x == 3 || (abs y == 5 && abs x < 4) = Wall
              | x == 0 && abs y < 4                     = Storage
              | x == -1 && (y == 0 || abs y == 2)       = Box
              | x == 1 && (abs y == 1 || abs y == 3)    = Box
              | x == (-2) && y == 1                     = Wall
              | otherwise                               = Ground

maze7 :: Coord -> Tile
maze7 (C x y) | abs x > 4 || abs y > 4   = Blank
              | abs x == 4 || abs y == 4 = Wall
              | not (x == 2) && y == 2   = Wall
              | not (x == -2) && y == -1 = Wall
              | x == 3 && y == -3        = Storage
              | x == 2 && y == 2         = Box
              | otherwise                = Ground

maze8 :: Coord -> Tile
maze8 (C x y) | abs x > 10 || abs y > 10    = Blank
              | x == 0 && y == 0            = Ground
              | abs x == 9 && abs y == 9    = Wall
              | abs x == 10 || abs y == 10  = Wall
              | x == y                      = Storage
              | abs x == abs y              = Box
              | x < 0 && x > (-9) && y == 0 = Box
              | x > 0 && x < 9 && y == 0    = Storage
              | otherwise                   = Ground

maze9 :: Coord -> Tile
maze9 (C x y) | abs x > 4 || abs y > 4                  = Blank
              | abs x == 4 || abs y == 4 || x == -3     = Wall
              | x == -2 && (y == 3 || y == 0)           = Wall
              | x == -1 && y == -1                      = Wall
              | x == -0 && y == 1                       = Wall
              | x == 3 && y == 0                        = Wall
              | x < 0 && (y == 2 || y == -3)            = Storage
              | x == -1 && y == 1                       = Storage
              | x == 0 && (y == 2 || y == 0 || y == -1) = Box
              | x == 1 && y == -2                       = Box
              | x == 2 && y == -3                       = Box
              | otherwise                               = Ground

maze4'' :: Coord -> Tile
maze4'' (C 1 (-3)) = Box
maze4'' c          = maze4 c

maze4' :: Coord -> Tile
maze4' (C 0 1) = Blank
maze4' c       = maze4 c

maze9' :: Coord -> Tile
maze9' (C 3 0) = Box
maze9' (C 4 0) = Box
maze9' c       = maze9 c

