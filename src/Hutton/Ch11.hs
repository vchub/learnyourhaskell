{-# OPTIONS_GHC -Wall #-}

module Hutton.Ch11 where

import           Data.Char
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

valid :: Grid -> Int -> Bool
valid g n = 0 <= n && n < size * size && concat g !! n == B

move :: Grid -> Int -> Player -> [Grid]
move g n p = if valid g n then [chop size (xs ++ [p] ++ ys)] else []
  where (xs, _ : ys) = splitAt n (concat g)

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat promt = do
  putStr promt
  xs <- getLine
  if xs /= [] && all isDigit xs
    then return (read xs)
    else do
      putStrLn "ERROR: Invalid number"
      getNat promt

tictaktoe :: IO ()
tictaktoe = run empty O

run :: Grid -> Player -> IO ()
run g p = do
  cls
  goto (1, 1)
  putGrid g
  run' g p

run' :: Grid -> Player -> IO ()
run' g p
  | wins O g = putStrLn "Player O won!\n"
  | wins X g = putStrLn "Player X won!\n"
  | full g = putStrLn "It's a draw.\n"
  | otherwise = do
    i <- getNat (prompt p)
    case move g i p of
      [] -> do
        putStrLn "ERROR: Invalid move"
        run' g p
      [g'] -> run g' (next p)
      _    -> putStrLn "ERROR: 502"

prompt :: Player -> String
prompt p = "Player " ++ show p ++ " , enter your move "

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ (show y) ++ ";" ++ (show x) ++ "H")










