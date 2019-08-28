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


-- Map.update (\v-> |Nothing = 1|Just (v+1)) 1 m
-- m = Map.fromList $ map (\x -> (show x, x)) [1..4]
-- m
-- valSort = sortBy (flip compare `on` snd)
-- valSort $ Map.toList m
-- Map.insertWith (+) "6" 1 m

-- uf v


freq :: String -> Map.Map String Integer
freq = foldl (\m s ->  Map.insertWith (+) s 1 m) Map.empty . words

desc2 :: (Ord a, Ord b) => [(a,b)] -> [(a,b)]
desc2 = sortBy (flip compare `on` snd)

top :: (Ord a, Ord b)=> Int ->  Map.Map a b -> [(a, b)]
top n m = take n $ desc2 $ Map.toList m


main::IO()
main = do
  content <- getContents
  -- putStr $ fmap toUpper content
  putStr $ show $ top 5 $ freq content




