{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module CIS194.Homework2 where
import           CodeWorld

main :: IO ()
main = exercise1

data Tile = Wall| Ground| Storage|Box|Blank

drawTile :: Tile -> Picture
drawTile Wall    = colored grey (solidRectangle 1 1)
drawTile Ground  = colored yellow (solidRectangle 1 1)
drawTile Storage = solidCircle 0.3 & drawTile Ground
drawTile Box     = colored brown (solidRectangle 1 1)
drawTile Blank   = blank

drawTileAt :: Integer -> Integer -> Picture
drawTileAt x y =
  translated (fromIntegral x) (fromIntegral y) (drawTile (maze x y))

pictureOfMaze :: Picture
pictureOfMaze = foldl (\acc (x, y) -> drawTileAt x y & acc)
                      blank
                      [ (x, y) | x <- [-10 .. 10], y <- [-10 .. 10] ]

maze :: Integer -> Integer -> Tile
maze x y | abs x > 4 || abs y > 4   = Blank
         | abs x == 4 || abs y == 4 = Wall
         | x == 2 && y <= 0         = Wall
         | x == 3 && y <= 0         = Storage
         | x >= -2 && y == 0        = Box
         | otherwise                = Ground

data Direction = R | U | L | D

data Coord = C Integer Integer

initialCoord :: Coord
initialCoord = C 0 0

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x + 1) y
adjacentCoord U (C x y) = C x (y + 1)
adjacentCoord L (C x y) = C (x - 1) y
adjacentCoord D (C x y) = C x (y - 1)


handleEvent :: Event -> Coord -> Coord
handleEvent (KeyPress key) c | key == "Right" = adjacentCoord R c
                             | key == "Up"    = adjacentCoord U c
                             | key == "Left"  = adjacentCoord L c
                             | key == "Down"  = adjacentCoord D c
                             | otherwise      = c
handleEvent _ c = c

drawState :: Coord -> Picture
drawState c = atCoord c player & pictureOfMaze

player :: Picture
player =
  translated 0 0.3 cranium
    & polyline [(0, 0), (0.3, 0.05)]
    & polyline [(0, 0), (0.3, -0.05)]
    & polyline [(0, -0.2), (0, 0.1)]
    & polyline [(0, -0.2), (0.1, -0.5)]
    & polyline [(0, -0.2), (-0.1, -0.5)]
  where cranium = circle 0.18 & sector (7 / 6 * pi) (1 / 6 * pi) 0.18

exercise1 :: IO ()
exercise1 = activityOf initialCoord handleEvent drawState




