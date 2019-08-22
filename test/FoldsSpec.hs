{-# OPTIONS_GHC -Wall #-}

module FoldsSpec where
import           Test.Hspec
import           Test.Hspec.QuickCheck

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr f acc xs = case xs of
  []         -> acc
  (x : rest) -> myfoldr f (f x acc) rest

mysum :: (Num a) => [a] -> a
mysum = myfoldr (+) 0

spec :: Spec
spec = describe "FoldsSpec" $ do
  describe "myfoldr" $ do
    it "+" $ myfoldr (+) 0 [1, 2] `shouldBe` (3 :: Int)

  describe "mysum" $ do
    prop "sum of Int" $ \list -> mysum list `shouldBe` sum (list :: [Int])
    prop "sum of Double" $ \list -> mysum list `shouldBe` sum (list :: [Double])


