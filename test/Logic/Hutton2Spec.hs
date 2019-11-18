module Logic.Hutton2Spec where

-- import           Data.Char
import           Data.List
-- import           Data.List.Ordered
import           Test.Hspec
import           Test.Hspec.QuickCheck
-- import qualified Test.QuickCheck.Gen           as Gen
-- import           Test.QuickCheck

evens :: [Int] -> [Int]
evens []       = []
evens (x : xs) = x : odds xs

odds :: [Int] -> [Int]
odds []       = []
odds (_ : xs) = evens xs

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort l) (msort r)
 where
  (l, r) = halve xs
  n      = div (length xs) 2
  halve xs = (take n xs, drop n xs)
  merge :: Ord a => [a] -> [a] -> [a]
  merge [] y  = y
  merge x  [] = x
  merge (x : xs) (y : ys) | x <= y    = x : merge xs (y : ys)
                          | otherwise = y : merge (x : xs) ys


spec :: Spec
spec = describe "Hutton book" $ do
  describe "msort" $ do
    it "" $ msort [2, 1] `shouldBe` [1, 2]
    it "" $ msort [2, 1, 0] `shouldBe` [0, 1, 2]
    prop "sort msort" $ \xs -> msort xs `shouldBe` (sort xs :: [Int])

  describe "evens, odds" $ do
    it "" $ evens [1 .. 4] `shouldBe` [1, 3]
    it "" $ evens [1, 2] `shouldBe` [1]
    it "" $ evens [1] `shouldBe` [1]
    it "" $ odds [1 .. 4] `shouldBe` [2, 4]
