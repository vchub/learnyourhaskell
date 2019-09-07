{-# OPTIONS_GHC -Wall #-}
module CIS194.Homework4Spec where

-- import           CIS194.Homework4
import           Test.Hspec
-- import           Test.Hspec.QuickCheck
-- import           Data.List
-- import           CodeWorld

-- lightTimes :: (Num a) => a -> a -> a -> a
-- lightTimes t period dt
--   | (round t `mod` period :: Int) == 0 = frameBox black yellow black


spec :: Spec
spec = describe "Exercises" $ do
  describe "dummy" $ do
    it "yellow" $ 1 `shouldBe` (1 :: Int)
