{-# OPTIONS_GHC -Wall #-}
module Bird.Ch1 where

import           Control.Applicative
import           Data.List
import qualified Data.Map            as M
import qualified Data.Text           as T

count :: (Eq a) => a -> [a] -> Int
count a xs = sum [ 1 | x <- xs, x == a ]

squareCounter :: [String] -> M.Map String Int
squareCounter ss = go M.empty ss
 where
  go m []       = m
  go m (x : xs) = go (M.insertWith (+) x 1 m) xs

squareCount :: [String] -> [(String, Int)]
squareCount ss =
  sortBy (\(_, a) (_, b) -> compare b a) (M.toList (squareCounter ss))

countRuns :: T.Text -> [(T.Text, Int)]
-- countRuns::T.Text->[T.Text]
countRuns =
  (sortBy (\(_, x) (_, y) -> compare y x))
    . go
    . sort
    . (map T.toLower)
    . T.words
 where
  go []       = []
  go (x : xs) = (x, length l) : go r where (l, r) = break (x <) (x : xs)

numName :: M.Map Int String
numName = M.fromList
  [ (1 , "one")
  , (2 , "two")
  , (3 , "three")
  , (11, "eleven")
  , (12, "twelve")
  , (20, "twenty ")
  ]

fromPQ :: Int -> Int -> Maybe String
fromPQ n 100   = M.lookup n numName
fromPQ n 1000  = fmap (++ " hundred ") (M.lookup n numName)
fromPQ n 10000 = fmap (++ " thousand ") (M.lookup n numName)
fromPQ _ _     = error "fromPQ not implemented"
-- fromPQ p 20   = M.lookup p numName
-- fromPQ p 10  = M.lookup (10 + p) numName

sayNumber :: Int -> Maybe String
sayNumber x = go x 100 (Just "")
 where
  go :: Int -> Int -> Maybe String -> Maybe String
  go 0 _ acc = acc
  go n i acc
    | n < 20 = pure (++) <*> (fromPQ n i) <*> acc
    | n < 100 = foldl (liftA2 (++))
                      acc
                      [(fromPQ ((div n 10) * 10) 100), (fromPQ (mod n 10) i)]
    |
    -- TODO: that's wrong
      otherwise = go (div n 100)
                     (i * 10)
                     (pure (++) <*> (fromPQ (mod n i) i) <*> acc)


-- (pure (++) <*> (Just "some") <*> Just " foo")
-- a = Just 2
-- b = Just 2
-- pure (*) <*> a <*> b <*> a
-- foldl (liftA2 (+)) a [a,b,a]

{--
break (1<) [1,3,4]
break (==4) [1,3,4]
break (==4) [4]
break (==4) [4]
--}

-- ====================
-- Given a sorted array arr of distinct integers, return the lowest index i for
-- which arr[i] == i. Return null if there is no such index.
--
-- For example, given the array [-5, -3, 2, 3], return 2 since arr[2] == 2.
-- Even though arr[3] == 3, we return 2 since it's the lowest index.

arri :: [Int] -> Maybe Int
-- arri xs = go 0 (length xs) -- (length xs - 1)
arri xs = go 0 (length xs - 1)
 where
  go :: Int -> Int -> Maybe Int
  go l h | xs !! l > l  = Nothing
         | xs !! h < h  = Nothing
         | xs !! l == l = Just l
         | xs !! m >= m = go l m
         | otherwise    = go (m + 1) h
    where m = (l + h) `div` 2


-- | l >= h       = Nothing






















