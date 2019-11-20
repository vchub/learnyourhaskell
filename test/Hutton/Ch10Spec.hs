{-# OPTIONS_GHC -Wall #-}

module Hutton.Ch10Spec where


-- import           Data.List
import           Hutton.Ch10
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = describe "Ch10" $ do

  describe "misc" $ do
    it "" $ intersect "" "aa" `shouldBe` "--"
    it "" $ intersect "a" "xaba" `shouldBe` "-a-a"
    it "" $ intersect "foo" "somef" `shouldBe` "-o--f"

    -- it "" $ someFunc `shouldBe` (0 :: IO Int)
    it "head is the 1st elem of a list" $ do
      property $ \x xs -> head (x : xs) == (x :: Int)
