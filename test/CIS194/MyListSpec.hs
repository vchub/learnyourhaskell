module CIS194.MyListSpec where

import           CIS194.MyList
import           Test.Hspec
import           Test.Hspec.QuickCheck
-- import           Data.List
-- import           CodeWorld

-- lightTimes :: (Num a) => a -> a -> a -> a
-- lightTimes t period dt
--   | (round t `mod` period :: Int) == 0 = frameBox black yellow black


spec :: Spec
spec = describe "MyList" $ do
    describe "fst' fstL and derivatives" $ do
      let xs = Entry 1 $ Entry 2 Empty
       in do
        it "fst'" $ fstL xs `shouldBe` 1
        it "toList" $ toList xs `shouldBe` [2,1]
        it "mapL" $ toList (mapL (+ 1 ) xs) `shouldBe` [2,3]
        it "fromList" $ toList (fromList  [1,2]) `shouldBe` [1,2]
        prop "toList . fromList xs == xs"
          $ \s -> toList (fromList s) `shouldBe` (s::[Int])
