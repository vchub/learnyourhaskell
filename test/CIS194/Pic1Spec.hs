{-# OPTIONS_GHC -Wall #-}

module CIS194.Pic1Spec where
import           Test.Hspec
-- import           Test.Hspec.QuickCheck
-- import           Data.List
-- import           CodeWorld

-- lightTimes :: (Num a) => a -> a -> a -> a
-- lightTimes t period dt
--   | (round t `mod` period :: Int) == 0 = frameBox black yellow black


spec :: Spec
spec = describe "Misc" $ do
  describe "dummy" $ do
    it "dummy" $ (round (1.0 :: Double)) `shouldBe` (1 :: Int)
