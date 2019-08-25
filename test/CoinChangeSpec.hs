{-# OPTIONS_GHC -Wall #-}
module CoinChangeSpec where
import           Test.Hspec
-- import           Test.Hspec.QuickCheck

-- https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/coinChange

-- given a list of coins [c1,..,cn] and a amount a we shall find a list
-- [a1,..,an] of integers such that c1*a1 + .. + cn*an = a - but not just any -
-- we want to minimize the number of coins given:  a1+..+an.

-- [[i_c1...i_cn]]  - indexes of coins in a change
type Changes = [[Integer]]

allChanges0 :: Integer -> [Integer] -> Changes
allChanges0 amount0 coinset0 = m amount0 coinset0 []
 where
  m :: Integer -> [Integer] -> [Integer] -> Changes
  m amount coinset acc
    | amount < 0
    = [[]]
    | amount == 0
    = [acc]
    | length coinset == 0
    = [[]]
    | otherwise
    = filter (\x -> length x /= 0)
      $  (m (amount - head coinset) coinset ((head coinset) : acc))
      ++ (m amount (tail coinset) acc)


-- allChanges :: Int -> [Int] -> Maybe Changes
-- allChanges amount0 coinset = m amount0 0
--  where
--   n = length coinset
--   m :: Int -> Int -> Maybe Changes
--   m amount ic
--     | amount < 0
--     = Nothing
--     | amount == 0
--     = Just [[]]
--     | ic >= n
--     = Nothing
--     | otherwise
--     = let m1 = fmap (ic :) (m (amount - coinset !! ic) ic)
--           m2 = fmap (ic :) (m amount (ic + 1))
--       in  case (m1, m2) of
--             (Nothing, r2     ) -> r2
--             (r1     , Nothing) -> r1
--             (Just r1, Just r2) -> Just $ r1 ++ r2

spec :: Spec
spec = describe "CoinChangeSpec" $ do
  describe "change" $ do
    it "1 [1]" $ allChanges0 1 [1] `shouldBe` [[1]]
    it "2 [1]" $ allChanges0 2 [1] `shouldBe` [[1, 1]]
    it "2 [1,2]" $ allChanges0 2 [1, 2] `shouldBe` [[1, 1], [2]]




-- x = Just [1, 2]
-- y = Just [3, 4]
-- Data.Foldable.foldl (fmap ++ []) Just [] [x y]

-- x = Just [[1, 2]]
-- fmap (0 :) x
-- fmap (\e -> (0:e)) x

-- let date _ = Time.getClockTime >>= print >>= return
-- let date _ = Time.getClockTime >>= print >> return ""

-- import Time



