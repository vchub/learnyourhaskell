{-# OPTIONS_GHC -Wall #-}
module WordsCountSpec where

-- import           Data.Char
import qualified Data.Map      as Map
-- import Data.Heap
import           Data.Function (on)
import           Data.List     (sortBy)
import           Test.Hspec

spec :: Spec
spec = describe "WordsCountSpec" $ do
  describe "misc" $ do
    it "assoiative list" $ 1 `shouldBe` (1 :: Int)
    let
      al :: [(Int, String)]
      al = [(1, "one"), (2, "two"), (3, "three"), (4, "four")]
           in do
                it "lookup by fst" $ lookup 1 al `shouldBe` Just "one"
                it "lookup by 5" $ lookup 5 al `shouldBe` Nothing

    it "Map" $ 1 `shouldBe` (1 :: Int)
    let
      m :: Map.Map Int String
      m = Map.fromList [(1, "one"), (2, "two"), (3, "three"), (4, "four")]
           in do
                it "lookup by fst" $ Map.lookup 1 m `shouldBe` Just "one"
                it "lookup by 5" $ Map.lookup 5 m `shouldBe` Nothing

  describe "desc2" $
    let
      l::[(String, Integer)]
      l = [("1",1),("2",2),("3",3),("4",4)]
      got = take 2 $ desc2 l
      want = [("4",4),("3",3)]
      in
        it "1" $ got  `shouldBe` want

  describe "word freq in a string" $ do
    it "1" $ freq "foo bar foo" `shouldBe` Map.fromList [("foo", 2),("bar", 1)]
    it "1" $ top 2 (freq "foo bar foo") `shouldBe` [("foo", 2),("bar", 1)]

  describe "Tree" $ do
    let testTree::Tree Int
        testTree = Node 5
            (Node 3
                (Node 1 Empty Empty)
                (Node 6 Empty Empty)
            )
            (Node 9
                (Node 8 Empty Empty)
                (Node 10 Empty Empty)
            )
     in do
        it "sum" $ foldl (+) 0 testTree `shouldBe` 42
        it "product" $ foldl (*) 1 testTree `shouldBe` 64800
        it "toList" $ foldr (:) [] testTree `shouldBe` [1,3,6,5,8,9,10]
        it "any > 15" $ foldl (\acc x-> acc || x > 15) False testTree `shouldBe` False
        it "any == 3" $ foldl (\acc x-> acc || x == 3) False testTree `shouldBe` True

  describe "primes" $ do
    it "start" $ take 11 primes `shouldBe` [2,3,5,7,11,13,17,19,23,29,31]

freq :: String -> Map.Map String Integer
freq = foldl (\m s ->  Map.insertWith (+) s 1 m) Map.empty . words

desc2 :: (Ord a, Ord b) => [(a,b)] -> [(a,b)]
desc2 = sortBy (flip compare `on` snd)

top :: (Ord a, Ord b)=> Int ->  Map.Map a b -> [(a, b)]
top n m = take n $ desc2 $ Map.toList m


main::IO()
main = do
  content <- getContents
  putStr $ show $ top 5 $ freq content


data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq, Read)

-- :{
instance Foldable Tree where
  foldMap _ Empty        = mempty
  foldMap f (Node a l r) = (foldMap f l) `mappend` f a `mappend` (foldMap f r)
-- :}

primes :: [Int]
primes = 2:3:(filter nondividable odds)
  where nomod n nums = all (\x-> n `mod` x /= 0) nums
        nondividable n = nomod n (takeWhile (<(div) n 2) primes)
        odds = [5,7..]

-- nats = (2:[3,5..])
-- takeWhile (< 40) nats


-- toInteger . round . sqrt 4
-- (div) 4 2

-- nomod 6 [4, 6]
-- nomod 6 [4]
--
-- (mod) 6 4
-- fmap (*3) (+100)
-- fn = show . (*3). (+100)
-- fn 3
--
-- fn = fmap (++" !")
-- fn $ Just "some"
-- fn $ Nothing
-- fn $ Left "foo"
-- fn $ Right "foo"
--
-- import Control.Applicative
--
-- fn = fmap (++) (Just "ho")
-- fn <*> Just "foo"
-- fmap fn (Just "foo")
--
-- succ <$> [1..5]
-- [succ] <*> [1..5]
-- (++) <$> Just "one" <*> Just "two"
-- foldl (\acc x -> (++) <$> acc <*> x ) Nothing [Just "one" ,Just "two"]
-- foldl (\acc x -> (++) <$> acc <*> x ) (Just "") [Just "one" ,Just "two"]
--
-- (+) <$> (+3) <*> (*100) $ 5
-- [(+3),(*2),(/2)] <*> [5]
--
--
-- -- Monoids
-- mappend [12,1] [3,4]
-- mconcat (++) [[1,2], [3,4]]
-- mconcat [[1,2], [3,4]]
-- mempty :: [a]
-- mappend mempty [1] == mappend [1] mempty
-- (mappend [1,2] $ mappend [3] [4]) == (mappend (mappend [1,2] [3]) [4])
--
-- newtype Any = Any { getAny :: Bool }  deriving (Eq, Ord, Read, Show, Bounded)
-- map Any [False, True]
--
