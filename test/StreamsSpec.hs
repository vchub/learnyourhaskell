{-# OPTIONS_GHC -Wall #-}

module StreamsSpec where
import           Test.Hspec

ones :: [Integer]
ones = 1 : ones

numsFrom :: Integer -> [Integer]
numsFrom n = n : numsFrom (n + 1)

fibs :: [Integer]
fibs = [1, 1] ++ zipWith (+) fibs (tail fibs)

-- putId :: Int -> Int
-- putId x = do
--   putStrLn "foo"
--   x

spec :: Spec
spec = describe "StreamsSpec" $ do
  describe "ones" $ do
    it "ones" $ take 3 ones `shouldBe` ([1, 1, 1] :: [Integer])

  describe "numsFrom" $ do
    it "0,1.." $ (take 3 (numsFrom 0)) `shouldBe` ([0, 1, 2] :: [Integer])
    it "-2,-1.."
      $          (take 4 (numsFrom (-2)))
      `shouldBe` ([-2, -1, 0, 1] :: [Integer])

  describe "fibs" $ do
    it "1,1,2.." $ take 3 fibs `shouldBe` ([1, 1, 2] :: [Integer])
    it "1,1,2..21"
      $          take 8 fibs
      `shouldBe` ([1, 1, 2, 3, 5, 8, 13, 21] :: [Integer])

  -- describe "lambda with do" $ do
  --   it "print?" (putId 1) `shouldBe` 1
