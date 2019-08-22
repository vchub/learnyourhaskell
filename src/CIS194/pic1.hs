{-# OPTIONS_GHC -Wall #-}
import           CodeWorld


-- botCircleGreen c = colored green (translated 0 (-1.5) (solidCircle 1))
-- botCircleGreen c = colored c (translated 0 (-1.5) (solidCircle 1))
-- topCircleRed = colored red (translated 0 1.5 (solidCircle 1))
-- topCircleRed c = colored c (translated 0 1.5 (solidCircle 1))

-- trafficLight :: Bool -> Picture
-- trafficLight True = frameBox

frameBox :: Color -> Color -> Color -> Picture
frameBox tc mc bc =
  (rectangle 2.5 8.5) & (lamp tc 3) & (lamp mc 0) & (lamp bc (-3))
  where lamp c y = colored c (translated 0 y (solidCircle 1))

trafficController :: Double -> Picture
trafficController t = case dt of
  0 -> frameBox red black black
  1 -> frameBox black black green
  _ -> frameBox black yellow black
-- trafficController t | dt == 0   = frameBox red black black
--                     | dt == 1   = frameBox black black green
--                     | otherwise = frameBox black yellow black
  where dt = (round t `mod` 3 :: Int)

{-
   spread :: Picture -> Double -> Int -> Picture
   spread _   _  0 = blank
   spread pic dx n = pic & translated 0 dx (spread pic dx (n - 1))
-}

main :: IO ()
main = animationOf trafficController
-- main = animationOf spr where spr t = spread (trafficController t) 5 2


-- (max 4) 5
-- sqrt 2
