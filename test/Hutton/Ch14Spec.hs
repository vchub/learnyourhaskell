{-# OPTIONS_GHC -Wall #-}

module Hutton.Ch14Spec where

-- import           Control.Exception
-- import           Data.Char
import           Data.Foldable
import           Data.Monoid
import           Hutton.Ch14
import           Test.Hspec
-- import           Test.QuickCheck


spec :: Spec
spec = describe "Ch14" $ do
  describe "Misc" $ do
    it "Foldable" $ getSum (foldMap Sum [1 .. 10]) `shouldBe` (55 :: Int)
    it "Foldable" $ getProduct (foldMap Product [1 .. 4]) `shouldBe` (24 :: Int)
    let t::Tree Int
        t = Node (Node (Node Leaf 1 Leaf) 3 Leaf) 5 (Node Leaf 7 Leaf)
        t1::Tree Int
        t1= fromList [1..5]
        tjust = fromList (fmap Just [1..5])
     in do
      it "sum1 tree" $ sum1 t `shouldBe` sum [1,3,5,7]
      it "sum tree" $ sum t `shouldBe` sum [1,3,5,7]
      it "product tree" $ product t `shouldBe` product [1,3,5,7]
      it "product tree" $ product t `shouldBe` product [1,3,5,7]
      it "toList tree" $ toList t `shouldBe` [1,3,5,7]
      it "length tree" $ length t `shouldBe` 4
      it "elem tree" $ elem 4 t `shouldBe` False
      it "elem tree" $ elem 5 t `shouldBe` True
      it "maximum tree" $ maximum t `shouldBe` 7
      it "foldr + " $ foldr (+) 0 t `shouldBe` sum [1,3,5,7]
      it "average" $ average t `shouldBe` average [1,3,5,7]
      it "all" $ all (<= 7) t `shouldBe` True
      it "concat" $ concat (fromList [[1,2],[3,4],[5]]) `shouldBe` ([1..5]::[Int])
      it "traverse" $ traverse (\x-> Just (x+1)) [1..4] `shouldBe` (Just [2..5]::Maybe [Int])
      it "Functor" $ toList (fmap succ t) `shouldBe` [2,4,6,8]
      it "Functor" $ toList (fmap pred t1) `shouldBe` [0..4]

      it "traverse" $ fmap toList (traverse (\x-> Just (succ x)) t1) `shouldBe` (Just [2..6]::Maybe [Int])
      it "sequenceA" $ fmap toList (sequenceA tjust) `shouldBe` (Just [1..5]::Maybe [Int])
      it "sequenceA" $ fmap toList (sequenceA (fromList [Just 1, Nothing])) `shouldBe` (Nothing::Maybe [Int])

  describe "ch15 Exercises" $ do
    let eps::Float
        eps = 1.0e-3
        close x y  = abs (x-y) < eps
     in do
      it "sqrt1" $ close (sqrt 2.0) (sqrt1 2.0 eps) `shouldBe` True




