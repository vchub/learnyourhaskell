{-# OPTIONS_GHC -Wall #-}

module CIS194.Maze where
-- import qualified Data.Map                      as Map
import           CodeWorld


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
-- pictureOfMaze = undefined
pictureOfMaze = foldl (\acc (x, y) -> drawTileAt x y & acc)
                      blank
                      [ (x, y) | x <- [-10 .. 10], y <- [-10 .. 10] ]

exercise3 :: IO ()
exercise3 = drawingOf pictureOfMaze

maze :: Integer -> Integer -> Tile
maze x y | abs x > 4 || abs y > 4   = Blank
         | abs x == 4 || abs y == 4 = Wall
         | x == 2 && y <= 0         = Wall
         | x == 3 && y <= 0         = Storage
         | x >= -2 && y == 0        = Box
         | otherwise                = Ground



-- data Point = Point
--     { pointX, pointY :: Double
--     , pointName      :: String
--     } deriving (Show)


-- wall, ground, storage, box :: Picture
-- wall = colored grey (solidRectangle 1 1)
-- ground = colored yellow (solidRectangle 1 1)
-- storage = solidCircle 0.3 & ground
-- box = colored brown (solidRectangle 1 1)

