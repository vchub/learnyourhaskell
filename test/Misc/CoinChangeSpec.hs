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

  describe "MyEither Eq" $ do
    it "1 0 == 1 0" $ (L 1 :: MyEither Integer Integer) == L 1 `shouldBe` True
    it "1 _ == 1 message"
      $          (L 1 :: MyEither Integer String)
      ==         L 1
      `shouldBe` True
    it "_ a == _ a"
      $          (R "foo" :: MyEither Integer String)
      ==         R "foo"
      `shouldBe` True

  describe "MyEither Functor" $ do
    it "1 -> 2 " $ fmap succ (R 1 :: MyEither Integer Integer) `shouldBe` R 2

  describe "Pair Functor" $ do
    it "1 -> 2 " $ fmap succ (Pair 1 2 :: Pair Integer) `shouldBe` Pair 2 3

  describe "Functor composition" $ do
    it "1 -> 2 -> 2^3 "
      $          fmap ((^ (2 :: Integer)) . succ) (Just 1 :: Maybe Integer)
      `shouldBe` Just 4
{-
   :{
   data MyEither2 a e = L a | R e deriving Show
   :}
-}

data MyEither a e = L a | R e
  deriving Show

-- l = L "some"
-- r = R 1
-- fmap succ r
-- <* R 2 1
-- [2] <* [3]

instance (Eq a, Eq e) =>Eq (MyEither a e) where
  L x == L y = x == y
  R x == R y = x == y
  _ == _ = False

instance Functor (MyEither b) where
  fmap _ (L x)=L x
  fmap f (R x)=R (f x)

data MyMaybe a = None | J a

instance Functor MyMaybe where
  fmap f (J x) = J (f x)
  fmap _ None  = None

data Pair a = Pair a a
  deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)



-- x = Just [1, 2]
-- y = Just [3, 4]
-- x y >>= \x y -> Just $ x ++ y
--
-- sum [Just 1, Just 2]
-- x = Just [[1, 2]]
-- fmap (0 :) x
-- fmap (\e -> (0:e)) x

-- let date _ = Time.getClockTime >>= print >>= return
-- let date _ = Time.getClockTime >>= print >> return ""

-- import Time
-- [1,2,3] >>= \x -> [x,x]
-- [1,2,3] >>= \x -> [x,x]

-- succ <$> [1, 2]
-- [succ, pred] <*> [1, 2]
-- zip [0,1] [1, 2]

-- zip [succ, pred] [1, 2]
-- (succ, 1)

-- zipWith (\f x-> f$x) [succ, pred] [1, 2]

-- newtype CharList = CharList {getCharList::String} deriving (Show, Eq)
-- CharList "foo" == CharList "foo"
--
-- head [1, undefined, undefined]
-- tail [1, undefined, undefined]
-- tail [1, 2, undefined, undefined]








