{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}

module CIS194.Homework1
  ( lightState
  )
where
-- import           Data.Map
-- import qualified Data.Map                      as Map
import           CodeWorld

main :: IO ()
main = exercise3

-- Fill in the blanks! (When I say blanks, I mean undefineds)

-- Exercise 1

-- by vlad

lightState :: Double -> Int
lightState t | dt <= 1   = 0
             | dt <= 2   = 1
             | dt <= 4   = 2
             | otherwise = 3
  where dt = (round t `mod` 6 :: Int)

frameBox :: Color -> Color -> Color -> Picture
frameBox tc mc bc =
  (rectangle 2.5 8.5) & (lamp tc 3) & (lamp mc 0) & (lamp bc (-3))
  where lamp c y = colored c (translated 0 y (solidCircle 1))

trafficController :: Double -> Picture
-- trafficController t | round (t / 3) `mod` 2 == 0 = trafficLight True
--                     | otherwise                  = trafficLight False
trafficController t = case (lightState t) of
  0 -> frameBox black black green
  1 -> frameBox black yellow black
  2 -> frameBox red black black
  _ -> frameBox black yellow black

-- by vlad

botCircle, topCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-1.5) (solidCircle 1))
topCircle c = colored c (translated 0 1.5 (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 5.5

trafficLight :: Bool -> Picture
trafficLight True  = botCircle green & topCircle black & frame
trafficLight False = botCircle black & topCircle red & frame

trafficLightAnimation :: Double -> Picture
trafficLightAnimation = trafficController

exercise1 :: IO ()
exercise1 = animationOf trafficLightAnimation


-- Exercise 2

tree :: Picture -> Integer -> Picture
tree b 0 = b
tree b n = polyline [(0, 0), (0, 1)] & translated
  0
  1
  (rotated (pi / 10) (tree b (n - 1)) & rotated (-pi / 10) (tree b (n - 1)))

leave :: Double -> Picture
leave t
  | dt < 10 = colored
    yellow
    (translated 0 0.1 (solidCircle (fromIntegral dt * 0.05)))
  | otherwise = blank
  where dt = round t `mod` 5

exercise2 :: IO ()
-- exercise2 = animationOf (leave 0)
exercise2 = animationOf (\t -> (tree (leave t) 8))


-- Exercise 3

wall, ground, storage, box :: Picture
wall = colored grey (solidRectangle 1 1)
ground = colored yellow (solidRectangle 1 1)
storage = solidCircle 0.3 & ground
box = colored brown (solidRectangle 1 1)

drawTileAt :: Integer -> Integer -> Picture
drawTileAt x y = translated (fromIntegral x) (fromIntegral y) (maze x y)


pictureOfMaze :: Picture
-- pictureOfMaze = undefined
pictureOfMaze = foldl (\acc (x, y) -> drawTileAt x y & acc)
                      blank
                      [ (x, y) | x <- [-10 .. 10], y <- [-10 .. 10] ]

exercise3 :: IO ()
exercise3 = drawingOf pictureOfMaze

maze :: Integer -> Integer -> Picture
maze x y | abs x > 4 || abs y > 4   = blank
         | abs x == 4 || abs y == 4 = wall
         | x == 2 && y <= 0         = wall
         | x == 3 && y <= 0         = storage
         | x >= -2 && y == 0        = box
         | otherwise                = ground


-- drawTile 1 = wall
-- drawTile 2 = ground
-- drawTile 3 = storage
-- drawTile 4 = box
-- drawTile _ = blank



-- data F = Int->Int
-- data I = Record Int Bool deriving (Show)
-- x = Record 1 True
-- x
-- :t x
-- :i x
-- :i I
-- :t Record
-- [x | x<-[1,2], False]
-- [not (x && y) | x<-[False, True], y<-[False, True]]
-- [x || y | x<-[False, True], y<-[False, True], x/=y]
-- [[x,y,z] | x<-[1..50],y<-[1..50],z<-[1..50], x**2 + y**2 == z**2, x <= y && y<= z]
-- head [1..]
-- map (\x -> (fst x) + (snd x)) (zip [1..3] [1..4])
-- map (\x-> \y-> x+y) (zip [1..3] [1..4])
-- [(x,y) | x<-[-1..3], y<-[1..2]]
--
-- (\x y -> x+y) 4 3
-- zipWith (+) [1..6] [1..4]
-- foldl (-) 0 [1..4]
-- foldr (-) 0 [1..4]
-- foldl max 0 [1,5,3]
-- foldr max 0 [1,5,3]
-- foldr (++) [] [[1],[2]]
-- foldl (++) [] [[1],[2]]
-- (:).
-- data C = A|B|C
--
-- type F = Int->Int


-- m = [(1, 10), (2,20)]
-- ml = \i->lookup i m
-- ml 2

