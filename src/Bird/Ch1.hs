{-# OPTIONS_GHC -Wall #-}
module Bird.Ch1 where

import           Data.List
import qualified Data.Map                      as M
import qualified Data.Text                     as T

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


{--
break (1<) [1,3,4]
break (==4) [1,3,4]
break (==4) [4]
break (==4) [4]
--}
