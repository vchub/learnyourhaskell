-- {-# OPTIONS_GHC -Wall #-}

module Hutton.CountdownSpec where


import           Test.Hspec

-- Given a sequence of numbers and a target number, attempt to construct an
-- expression whose value is the target, by combining one or more numbers from
-- the sequence using addition, subtraction, multiplication, division and
-- parentheses.

-- solution0::[Int]

data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n)      = show n
  show (App op x y) = brak x ++ show op ++ brak y
    where
      brak :: Expr -> String
      brak (Val n) = show n
      brak e       = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n     ) = [n]
values (App op l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [ n | n > 0 ]
eval (App op l r) =
  [ apply op x y | x <- values l, y <- values r, valid op x y ]

subs :: [a] -> [[a]]
subs []       = [[]]
subs (x : xs) = yss ++ map (x :) yss where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []       = [[x]]
interleave x (y : ys) = (x : y : ys) : map (y :) (interleave x ys)
-- interleave a xs = [ (take i xs) ++ [a] ++ (drop i xs) | i <- [0 .. length xs] ]

perm :: [a] -> [[a]]
perm []       = [[]]
-- perm [x     ] = [[x]]
perm (x : xs) = concat (map (interleave x) (perm xs))

choices :: [a] -> [[a]]
choices = concat . map perm . subs
-- choices []       = []
-- choices [x     ] = [[x]]
-- choices (x : xs) = [[x]] ++ choices xs ++ perm (x : xs)

spec :: Spec
spec = describe "Countdown 1" $ do

  describe "perm" $ do
    -- it ""
    --   $ eval (App Add (App Add (Val 2) (Val 3)) (App Add (Val 2) (Val 3)))
    --   `shouldBe` [5]

    it "" $ eval (App Add (Val 2) (Val 3)) `shouldBe` [5]

    it ""
      $          show (App Add (Val 1) (App Mul (Val 2) (Val 3)))
      `shouldBe` "1+(2*3)"
    it "" $ show Add `shouldBe` "+"
    it "" $ choices [2] `shouldBe` [[], [2]]
    it "" $ choices [1, 2] `shouldBe` [[], [2], [1], [1, 2], [2, 1]]
    it ""
      $          choices [1, 2, 3]
      `shouldBe` [ []
                 , [3]
                 , [2]
                 , [2, 3]
                 , [3, 2]
                 , [1]
                 , [1, 3]
                 , [3, 1]
                 , [1, 2]
                 , [2, 1]
                 , [1, 2, 3]
                 , [2, 1, 3]
                 , [2, 3, 1]
                 , [1, 3, 2]
                 , [3, 1, 2]
                 , [3, 2, 1]
                 ]


    it "" $ interleave 1 [] `shouldBe` [[1]]
    it "" $ interleave 1 [2] `shouldBe` [[1, 2], [2, 1]]
    it "" $ perm [2] `shouldBe` [[2]]
    it ""
      $          perm [1, 2, 3]
      `shouldBe` [ [1, 2, 3]
                 , [2, 1, 3]
                 , [2, 3, 1]
                 , [1, 3, 2]
                 , [3, 1, 2]
                 , [3, 2, 1]
                 ]
