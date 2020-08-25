{-# OPTIONS_GHC -Wall #-}
module Bird.Ch4 where

triad :: Int -> [(Int, Int, Int)]
triad n =
  [ (x, y, z)
  | x <- [1 .. m]
  , y <- [x + 1 .. n]
  , coprime x y
  , z <- [y + 1 .. n]
-- , x ^ (2 :: Int) + y ^ (2 :: Int) == z ^ (2 :: Int)
  , x * x + y * y == z * z
  ]
  where m = floor (fromIntegral n / sqrt (2 :: Float))

divisors :: Int -> [Int]
divisors n = [ x | x <- [2 .. n - 1], mod n x == 0 ]

coprime :: Int -> Int -> Bool
coprime x y = disjoint (divisors x) (divisors y)
 where
  disjoint []        _         = True
  disjoint _         []        = True
  disjoint (x' : xs) (y' : ys) = if x' /= y' then disjoint xs ys else False


