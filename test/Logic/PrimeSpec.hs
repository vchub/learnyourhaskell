{-# OPTIONS_GHC -Wall #-}

module Logic.PrimeSpec where
import           Test.Hspec
-- import           Test.Hspec.QuickCheck

prime0 :: Integer -> Bool
prime0 n | n < 1     = error "not a positive Integer"
         | n == 1    = False
         | otherwise = ldf 2 n == n
 where
  ldf i n' | i ^ (2 :: Integer) > n' = n'
           | mod n' i == 0           = i
           | otherwise               = ldf (succ i) n'

spec :: Spec
spec = describe "Primes" $ do
  describe "prime0" $ do
    it "+" $ foldl (+) 0 [1, 2] `shouldBe` (3 :: Int)
    it "[2,3,5,7,11]" $ all prime0 [2, 3, 5, 7, 11] `shouldBe` True
    it "[2,3,5,7,11,12]" $ all prime0 [2, 3, 5, 7, 11, 12] `shouldBe` False
    it "[10,12]" $ all (not . prime0) [10, 12] `shouldBe` True
