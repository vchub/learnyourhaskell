-- {-# OPTIONS_GHC -Wall #-}

module Hutton.CountdownSpec where


import           Data.List
import           Test.Hspec

-- Given a sequence of numbers and a target number, attempt to construct an
-- expression whose value is the target, by combining one or more numbers from
-- the sequence using addition, subtraction, multiplication, division and
-- parentheses.

-- solution0::[Int]

data Op = Add | Sub | Mul | Div deriving (Eq)

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && (x `mod` y) == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr deriving (Eq)

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

choices1 :: [a] -> [[a]]
choices1 ns = concat [ perm s | s <- subs ns ]

split :: [a] -> [([a], [a])]
split []       = []
split [_     ] = []
split (x : xs) = ([x], xs) : [ (x : ls, rs) | (ls, rs) <- split xs ]

solution :: Expr -> [Int] -> Int -> Bool
-- solution e ns n = elem (values e) (choices ns) && eval e == [n]
solution e ns n = elem (values e) (choices ns) && elem n (eval e)

exprs :: [Int] -> [Expr]
exprs []  = []
exprs [x] = [Val x]
exprs ns =
  [ e | (ls, rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r ]
  where combine l r = [ App op l r | op <- [Add, Sub, Mul, Div] ]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [ e | ns' <- choices ns, e <- exprs ns', eval e == [n] ]

type Result = (Expr, Int)


results :: [Int] -> [Result]
results []  = []
results [n] = [ (Val n, n) | n > 0 ]
results ns =
  [ res
  | (ls, rs) <- split ns
  , lx       <- results ls
  , ry       <- results rs
  , res      <- combine lx ry
  ]
 where
  combine (l, x) (r, y) =
    [ (App op l r, apply op x y) | op <- ops, valid op x y ]
  ops :: [Op]
  ops = [Add, Sub, Mul, Div]

solutions1 :: [Int] -> Int -> [Expr]
solutions1 ns n = [ e | ns' <- choices ns, (e, m) <- results ns', m == n ]

spec :: Spec
spec = describe "Countdown 1" $ do

  describe "solution1" $ do
    -- it "" $ solutions1 [4, 1, 3] 4 `shouldBe` []
    -- it "" $ solutions1 [1, 2, 3] 5 `shouldBe` []
    -- it "" $ solutions1 [1, 2, 3] 6 `shouldBe` []
    -- it "" $ solutions1 [1, 2, 3] 7 `shouldBe` [Val 1]
    -- it "" $ results [4, 1, 3] `shouldBe` []
    -- it "" $ length (solutions1 [1, 3, 7, 10, 25, 50] 765) `shouldBe` 780
    it "" $ length (solutions1 [1, 3, 7, 10, 25, 50] 765) `shouldBe` 49

    describe "solution" $ do
      -- it "[1,2] 1" $ solutions [1, 2] 1 `shouldBe` [(Val 1)]
      -- it "3 [1,2] " $ solutions [1, 2] 3 `shouldBe` [(Val 1)]
      -- it "2 [1,2]" $ solutions [1, 2] 2 `shouldBe` [(Val 1)]
      -- it "2 [1,2,3]" $ solutions [1, 2, 3] 2 `shouldBe` [(Val 2)]
      -- it "" $ solutions [1, 2, 3] 7 `shouldBe` [(Val 1)]

      it "" $ solution (Val 1) [1, 2] 1 `shouldBe` True
      it "" $ solution (App Add (Val 1) (Val 2)) [1, 2] 3 `shouldBe` True
      -- it ""
      --   $ solution (App Mul (Val 3) (App Add (Val 1) (Val 2))) [1 .. 3] 6
      --   `shouldBe` True

      it "" $ split [1, 2, 3] `shouldBe` [([1], [2, 3]), ([1, 2], [3])]
      it ""
        $          split [1, 2, 3, 4]
        `shouldBe` [([1], [2, 3, 4]), ([1, 2], [3, 4]), ([1, 2, 3], [4])]

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

  describe "Exercise ch 9" $ do
    it "" $ choices1 [2] `shouldBe` [[], [2]]
    it "" $ choices1 [1, 2] `shouldBe` [[], [2], [1], [1, 2], [2, 1]]

    it "" $ isChoice ([] :: [Int]) ([] :: [Int]) `shouldBe` True
    it "" $ isChoice [1, 2] [3, 2, 1] `shouldBe` True
    it "" $ isChoice [3, 1, 2] [3, 2, 1] `shouldBe` True
    it "" $ isChoice [3, 4, 2] [3, 2, 1, 5] `shouldBe` False


isChoice :: Ord a => [a] -> [a] -> Bool
isChoice xs ys = subls (sort xs) (sort ys)
 where
  subls [] _  = True
  subls _  [] = False
  subls (x : xs) (y : ys) | x > y     = False
                          | x < y     = subls xs (y : ys)
                          | otherwise = subls xs ys






