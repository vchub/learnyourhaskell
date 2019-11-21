{-# OPTIONS_GHC -Wall #-}

module Hutton.Ch11 where

-- import           Data.Char
import           Data.List
-- import           System.IO

size :: Int
size = 3

data Player = O|B|X deriving (Eq,Ord)

instance Show Player where
  show O = "O"
  show B = " "
  show X = "X"

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

won :: Grid -> Bool
won g = wins O g || wins X g

transpose' :: [[a]] -> [[a]]
transpose' ([] : _) = []
transpose' xss      = [map head xss] ++ (transpose' (map tail xss))

-- diag :: Grid -> [Player]
diag :: [[a]] -> [a]
diag g = [ g !! i !! i | i <- [0 .. length g - 1] ]

interleave :: a -> [a] -> [a]
interleave _ []       = []
interleave _ [y     ] = [y]
interleave x (y : ys) = y : x : interleave x ys

showRow :: Show a => [a] -> String
showRow = concat . interleave bar . map show where bar = " | "

showGrid :: Grid -> String
showGrid = unlines . interleave bar . map showRow
  -- where bar = "----------------"
  where bar = concat $ replicate ((size * 4) - 1) "-"


putGrid :: Grid -> IO ()
putGrid = putStrLn . showGrid
-- putGrid = putStrLn . unlines . concat . interleave bar . map showRow
--   where bar = [replicate ((size * 4) - 1) '-']






