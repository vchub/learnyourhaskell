{-# OPTIONS_GHC -Wall #-}
module Bird.Ch4Spec where

-- import           Data.List     (sortBy)
import           Bird.Ch4
import           Test.Hspec

spec :: Spec
spec = describe "Ch4" $ do
  describe "triad" $ do
    it "triad" $ triad 15 `shouldBe` [(3, 4, 5), (5, 12, 13), (9, 12, 15)]
    it "divisors" $ divisors 4 `shouldBe` [2]
    it "divisors" $ divisors 10 `shouldBe` [2, 5]
    it "coprime" $ coprime 4 3 `shouldBe` True
    it "coprime" $ coprime 4 10 `shouldBe` False


