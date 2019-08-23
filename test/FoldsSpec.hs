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

substr :: (Ord a) => [a] -> [a] -> Bool
substr []      _  = True
substr (_ : _) [] = False
substr term@(x : xs) (y : ys) | x == y    = substr xs ys
                              | otherwise = substr term ys


spec :: Spec
spec = describe "FoldsSpec" $ do
  describe "myfoldr" $ do
    it "+" $ myfoldr (+) 0 [1, 2] `shouldBe` (3 :: Int)

  describe "mysum" $ do
    prop "sum of Int" $ \list -> mysum list `shouldBe` sum (list :: [Int])
    prop "sum of Double" $ \list -> mysum list `shouldBe` sum (list :: [Double])

  describe "substr" $ do
    it "empty is substr of empty" $ substr ([] :: [Integer]) [1] `shouldBe` True
    it "empty is substr [2,3]" $ substr ([] :: [Integer]) [2, 3] `shouldBe` True
    it "first symbols" $ substr "xs" "xss" `shouldBe` True
    it "not first symbols" $ substr "xs" "axss" `shouldBe` True
    it "not substr" $ substr "ys" "axss" `shouldBe` False
    prop "substr prop" $ \s -> substr s ("some" ++ s ++ "foo") `shouldBe` True




