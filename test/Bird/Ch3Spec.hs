{-# OPTIONS_GHC -Wall #-}
module Bird.Ch3Spec where

-- import           Data.List     (sortBy)
import           Bird.Ch3
import           Test.Hspec

spec :: Spec
spec = describe "Ch3" $ do
  describe "misc" $ do
    it "until0" $ until0 (> 10) (* 2) (1 :: Int) `shouldBe` 16
    it "until0" $ until0 (< 1) (/ 10) (10 :: Float) `shouldBe` 0.1
    it "until" $ until (< 1) (/ 10) (10 :: Float) `shouldBe` 0.1
    it "floor0" $ floor0 4.3 `shouldBe` 4
    it "floor0" $ floor0 0.3 `shouldBe` 0
    it "floor0" $ floor0 (-0.3) `shouldBe` -1
    it "floor0" $ floor0 (-5.8) `shouldBe` -6
    it "floor0" $ floor0 15.8 `shouldBe` 15
    it "floor0" $ floor0 (-15.8) `shouldBe` -16

    it "floor1" $ floor1 4.3 `shouldBe` 4
    it "floor1" $ floor1 0.3 `shouldBe` 0
    it "floor1" $ floor1 (-0.3) `shouldBe` -1
    it "floor1" $ floor1 (-120.3) `shouldBe` -121
    it "floor1" $ floor1 (120.3) `shouldBe` 120

  describe "Nat" $ do
    it "toInt" $ toInt (fromInteger 3) `shouldBe` 3
    it "toInt" $ toInt (fromInteger 3) + (fromInteger 3) `shouldBe` 6
    it "toInt" $ toInt (fromInteger 3) - (fromInteger 3) `shouldBe` 0
    it "toInt" $ toInt ((fromInteger 3) - (fromInteger 4)) `shouldBe` 0
    it "toInt" $ toInt ((fromInteger 7) - (fromInteger 4)) `shouldBe` 3
    it "toInt" $ toInt (fromInteger 3) * (fromInteger 3) `shouldBe` 9
    it "Ord" $ (Succ . fromInteger $ 3) < (fromInteger 4) `shouldBe` False
    it "Ord" $ (Succ . fromInteger $ 3) == (fromInteger 4) `shouldBe` True
    it "Ord" $ (Succ . fromInteger $ 3) < (fromInteger 5) `shouldBe` True

    it "divMod0"
      $          divMod0 (Succ . fromInteger $ 7) (fromInteger 3)
      `shouldBe` (fromInteger 2, fromInteger 2)

  describe "isqrt" $ do
    it "isqrt" $ isqrt 3.4 `shouldBe` 1
    it "isqrt" $ isqrt 4.4 `shouldBe` 2
    it "isqrt" $ isqrt 32.4 `shouldBe` 5
    it "isqrt" $ isqrt 36 `shouldBe` 6
