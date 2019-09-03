{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds #-}

module Misc.ReversePolishNotationSpec where
-- import           Text.Read
import qualified Data.List  as List
-- import           System.Random
import           Test.Hspec
-- import           Test.Hspec.QuickCheck

-- http://learnyouahaskell.com/functionally-solving-problems

calculate :: String -> Double
calculate = head . (\ws -> cal' ws []) . words

cal' :: [String] -> [Double] -> [Double]
cal' []       stack = stack
cal' (s : ss) stack = case reads s :: [(Double, String)] of
  [(x, _)] -> cal' ss (x : stack)
  _        -> cal' ss (handle s stack)
   where
    handle :: String -> [Double] -> [Double]
    handle op (x : y : rest) = ((toOp op) x y) : rest
    handle _  _              = undefined


toD :: String -> Double
toD = \x -> read x :: Double

toOp :: Floating a => String -> a -> a -> a
toOp "+"  = (+)
toOp "-"  = (-)
toOp "*"  = (*)
toOp "**" = (**)
toOp _    = undefined

solveRPN :: (Num a, Read a) => String -> a
solveRPN = head . foldl ff [] . words
 where
  ff :: (Num a, Read a) => [a] -> String -> [a]
  ff (x : y : ys) "+" = (x + y) : ys
  ff (x : y : ys) "-" = (x - y) : ys
  ff (x : y : ys) "*" = (x * y) : ys
  -- ff (x : y : ys) "/" = (x / y) : ys
  -- ff (x : y : ys) "**" = (x ** y) : ys
  ff ys           s   = (read s) : ys

rpnExt :: String -> Double
rpnExt = head . foldl ff [] . words
 where
  ff :: [Double] -> String -> [Double]
  ff (x : y : ys) "+"   = (x + y) : ys
  ff (x : y : ys) "-"   = (x - y) : ys
  ff (x : y : ys) "*"   = (x * y) : ys
  ff (x : y : ys) "/"   = (x / y) : ys
  ff (x : y : ys) "**"  = (x ** y) : ys
  ff (x     : ys) "log" = (log x) : ys
  ff ys           "sum" = (sum ys) : []
  ff ys           s     = (read s) : ys

-- Heathrow to London
minTime :: (Num a, Ord a) => [a] -> a
minTime ns = minT $ mt ns (0, 0)
 where
  minT (a, b) = min a b
  mt :: (Num a, Ord a) => [a] -> (a, a) -> (a, a)
  mt (a : b : c : xs) (a0, b0) =
    mt xs $ (min (a0 + a) (b0 + b + c), min (b0 + b) (a0 + a + c))
  mt [a, b] (a0, b0) = ((a0 + a), (b0 + b))
  mt []     ab       = ab
  mt [_]    _        = undefined

spec :: Spec
spec = describe "ReversePolishNotation" $ do
  describe "minTime" $ do
    it "1 2" $ minTime [1, 2] `shouldBe` (1 :: Integer)
    it "1 2" $ minTime [1, 3, 1, 5, 1] `shouldBe` (3 :: Integer)
    it "1 2"
      $          minTime [50, 10, 30, 5, 90, 20, 40, 2, 25, 10, 8, 0]
      `shouldBe` (75 :: Integer)

  describe "read" $ do
    it "read 1" $ (read "1" :: Integer) `shouldBe` 1
    -- it "read +" $ (read "+" :: String) `shouldBe` "+"
    it "words 1 2 +" $ (words "1 2 +") `shouldBe` ["1", "2", "+"]

  describe "calculator" $ do
    it "calculate 1" $ calculate "1" `shouldBe` 1.0
    it "calculate 1 2 +" $ calculate "1 2 +" `shouldBe` 3.0
    it "calculate 3 1 2 + -" $ calculate "3 1 2 + -" `shouldBe` 0
    it "calculate 10 4 3 + 2 * -" $ calculate "10 4 3 + 2 * -" `shouldBe` 4
    it "calculate 10 4 3 + 2 * - 2 **"
      $          calculate "10 4 3 + 2 * - 2 **"
      `shouldBe` 16

  describe "solveRPN" $ do
    it "solveRPN 1" $ solveRPN "1" `shouldBe` (1 :: Integer)
    it "solveRPN 1 2 +" $ solveRPN "1 2 +" `shouldBe` (3 :: Integer)
    it "solveRPN 3 1 2 + -" $ solveRPN "3 1 2 + -" `shouldBe` (0 :: Integer)
    it "solveRPN 10 4 3 + 2 * -"
      $          solveRPN "10 4 3 + 2 * -"
      `shouldBe` (4 :: Integer)

  describe "rpnExt" $ do
    it "rpnExt 1" $ rpnExt "1" `shouldBe` 1
    it "rpnExt 10 4 3 + 2 * -" $ rpnExt "10 4 3 + 2 * -" `shouldBe` 4
    it "rpnExt 3 1 0 + log sum 3.0 /"
      $          rpnExt "3 1 0 + log sum 3.0 /"
      `shouldBe` 1

  describe "Tree" $ do
    it "[1]->"
      $          foldl insert Empty [1]
      `shouldBe` Node (1 :: Integer) Empty Empty
    it "[1,2,3,3]->"
      $          toList (foldl insert Empty ([1, 2, 3, 3] :: [Integer]))
      `shouldBe` [1, 2, 3]
    -- it "[1]->"
    --   $ let got = 1
    --         exp = 1
    --     in  got `shouldBe` exp

    it "[1,2,3,0]-> fmap succ"
      $ let xs   = ([1, 2, 3, 0] :: [Integer])
            t    = foldl insert Empty xs
            got  = toList $ fmap succ t
            want = List.sort $ fmap succ xs
        in  got `shouldBe` want

  describe "printIt" $ do
    it "printIt" $ printIt "foo"


data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

insert :: (Ord a) => Tree a -> a -> Tree a
insert Empty x = Node x Empty Empty
insert t@(Node v l r) x | x < v     = Node v (insert l x) r
                        | x > v     = Node v l (insert r x)
                        | otherwise = t

toList :: Tree a -> [a]
toList Empty        = []
toList (Node x l r) = (toList l) ++ [x] ++ (toList r)

instance Functor Tree where
  fmap _ Empty        = Empty
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

printIt :: String -> IO ()
printIt s = putStrLn s

-- take 3 $ randoms (mkStdGen 11)
-- take 3 $ randoms getStdGen
-- gen = mkStdGen
-- random gen::Int

-- printIn :: String -> String
-- printIn s = do
--   putStrLn s
--   s


-- splitAt 1 [1,2,3]
-- splitAt 2 [1,2,3]

-- read "1" :: Double
-- read "1" :: Double
-- reads "1 +" :: [(Double,String )]
-- reads "0.4" :: [(Double,String )]
-- reads ".4 +" :: [(Double,String )]
-- reads "1" :: [(Double,String )]
-- reads "+ 1" :: [(Double,String )]
-- reads "+1" :: [(Double,String )]
-- reads "1" :: [Double]



-- [1,"foo"]
-- (1,"foo")
-- show 3.4
-- words "3 4 +"
-- readMaybe "0.4" :: Maybe Double
-- readMaybe "0.4" :: Maybe String
-- readMaybe "+" :: Maybe Char
-- readMaybe "sss" :: Maybe Char
-- readMaybe "sss" :: Maybe String


-- import Data.Char
-- toUpper 'a'

-- data Frank a b  = Frank {frankField :: b a} deriving (Show)
-- x = Frank {frankField = Just 'a'}
-- x
-- :k x
-- :t x
-- :i x




-- case (1,2) of (x,y)->x+y
-- case [1,2] of [x,y]->x
-- case [1,2] of [x,y]->y
-- case [1,2] of (x:y:ys)->y

