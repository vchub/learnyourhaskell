{-# OPTIONS_GHC -Wall #-}

module Hutton.Ch11 where

-- import           Data.Char
import           Data.List
-- import           System.IO

size :: Int
size = 3

data Player = O|B|X deriving (Eq,Ord,Show)

type Grid = [[Player]]

next :: Player -> Player
next O = X
next B = B
next X = O

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
 where
  ps = concat g
  os = length $ filter (== O) ps
  xs = length $ filter (== X) ps

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ diags)
 where
  line  = all (== p)
  rows  = g
  cols  = transpose g
  diags = [diag g, diag (map reverse g)]

transpose' :: [[a]] -> [[a]]
transpose' ([] : _) = []
transpose' xss      = [map head xss] ++ (transpose' (map tail xss))

-- diag :: Grid -> [Player]
diag :: [[a]] -> [a]
diag g = [ (g !! i) !! i | i <- [0 .. length g - 1] ]


