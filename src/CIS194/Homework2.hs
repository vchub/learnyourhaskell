{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module CIS194.Homework2 where
import           CodeWorld

main :: IO ()
main = exercise1

data Tile = Wall| Ground| Storage|Box|Blank

data Direction = R | U | L | D

data Coord = C Integer Integer

data Player = Player Direction Coord

drawTile :: Tile -> Picture
drawTile Wall    = colored grey (solidRectangle 1 1)
drawTile Ground  = colored yellow (solidRectangle 1 1)
drawTile Storage = solidCircle 0.3 & drawTile Ground
drawTile Box     = colored brown (solidRectangle 1 1)
drawTile Blank   = blank

drawTileAt :: Coord -> Picture
drawTileAt (C x y) =
  translated (fromIntegral x) (fromIntegral y) (drawTile (maze (C x y)))

pictureOfMaze :: Picture
pictureOfMaze = foldl (\acc c -> drawTileAt c & acc)
                      blank
                      [ (C x y) | x <- [-10 .. 10], y <- [-10 .. 10] ]

maze :: Coord -> Tile
maze (C x y) | abs x > 4 || abs y > 4   = Blank
             | abs x == 4 || abs y == 4 = Wall
             | x == 2 && y <= 0         = Wall
             | x == 3 && y <= 0         = Storage
             | x >= -2 && y == 0        = Box
             | otherwise                = Ground

playerPic :: Picture
playerPic =
  translated 0 0.3 cranium
    & polyline [(0, 0), (0.3, 0.05)]
    & polyline [(0, 0), (0.3, -0.05)]
    & polyline [(0, -0.2), (0, 0.1)]
    & polyline [(0, -0.2), (0.1, -0.5)]
    & polyline [(0, -0.2), (-0.1, -0.5)]
  where cranium = circle 0.18 & sector (7 / 6 * pi) (1 / 6 * pi) 0.18

drawPlayer :: Player -> Picture
drawPlayer (Player R c) = atCoord c playerPic
drawPlayer (Player L c) = atCoord c (rotated pi playerPic)
drawPlayer (Player U c) = atCoord c (rotated (pi / 2) playerPic)
drawPlayer (Player D c) = atCoord c (rotated (pi * 3 / 2) playerPic)

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x + 1) y
adjacentCoord U (C x y) = C x (y + 1)
adjacentCoord L (C x y) = C (x - 1) y
adjacentCoord D (C x y) = C x (y - 1)

movePlayer :: Direction -> Player -> Player
movePlayer dir (Player d c) | isGround  = Player dir newCoord
                            | otherwise = (Player d c)
 where
  newCoord = adjacentCoord dir c
  isGround = case (maze newCoord) of
    Ground -> True
    _      -> False


handleEvent :: Event -> Player -> Player
handleEvent (KeyPress key) p | key == "Right" = movePlayer R p
                             | key == "Up"    = movePlayer U p
                             | key == "Left"  = movePlayer L p
                             | key == "Down"  = movePlayer D p
                             | otherwise      = p
handleEvent _ p = p

drawState :: Player -> Picture
drawState p = drawPlayer p & pictureOfMaze

initialPlayer :: Player
initialPlayer = Player L (C 0 1)

exercise1 :: IO ()
exercise1 = activityOf initialPlayer handleEvent drawState




