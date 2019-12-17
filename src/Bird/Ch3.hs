{-# OPTIONS_GHC -Wall #-}
module Bird.Ch3 where

-- myfloor :: Float -> Integer
-- myfloor = (`div` 1) . toInt

until0 :: (a -> Bool) -> (a -> a) -> a -> a
until0 p f a | p (f a)   = f (a)
             | otherwise = until0 p f (f a)

floor0 :: Float -> Int
floor0 x = if x < 0 then -1 * (succ ret) else ret
 where
  ret = go (abs x) 0
  go x' acc | x' < 1    = acc
            | otherwise = go (x' - 1) (acc + 1)


type Interval = (Integer, Integer)

floor1 :: Float -> Integer
floor1 x = fst (until unit shrink bound)
 where
  unit :: Interval -> Bool
  unit (m, n) = m + 1 == n

  shrink :: Interval -> Interval
  shrink (m, n) = if (fromInteger p) > x then (m, p) else (p, n)
    where p = div (m + n) 2

  bound :: Interval
  bound  = (loower, upper)
  loower = until ((<= x) . fromInteger) (* 2) (-1)
  upper  = until ((>= x) . fromInteger) (* 2) 1

data Nat = Zero | Succ Nat deriving (Eq, Ord, Show)

toInt :: Nat -> Integer
toInt Zero     = 0
toInt (Succ m) = 1 + (toInt m)


instance Num Nat where
  m + Zero     = m
  m + (Succ n) = Succ (m + n)
  m        - Zero     = m
  Zero     - _        = Zero
  (Succ m) - (Succ n) = m - n

  _ * Zero        = Zero
  m * (Succ Zero) = m
  m * (Succ n   ) = Succ (m + (n * m))
  fromInteger m | m <= 0    = Zero
                | otherwise = Succ . fromInteger $ m - 1
  abs m = m
  signum Zero     = Zero
  signum (Succ _) = Succ Zero
  negate m = m


