{-# OPTIONS_GHC -Wall #-}
module CIS194.Homework1Spec where
import           CIS194.Homework1               ( lightState )
import           Test.Hspec
-- import           Test.Hspec.QuickCheck
-- import           Data.List
-- import           CodeWorld

-- lightTimes :: (Num a) => a -> a -> a -> a
-- lightTimes t period dt
--   | (round t `mod` period :: Int) == 0 = frameBox black yellow black


spec :: Spec
spec = describe "Exercise 1" $ do
  describe "lightState" $ do
    it "green" $ lightState (1.0 :: Double) `shouldBe` (0 :: Int)
    it "green" $ lightState (0 :: Double) `shouldBe` (0 :: Int)
    it "yellow" $ lightState (2 :: Double) `shouldBe` (1 :: Int)
