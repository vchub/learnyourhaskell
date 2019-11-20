{-# OPTIONS_GHC -Wall #-}

module Hutton.Ch10
  ( strlen
  , intersect
  , hangman
  , act
  )
where

intersect :: String -> String -> String
intersect _ [] = []
intersect xs (y : ys) | elem y xs = y : intersect xs ys
                      | otherwise = '-' : intersect xs ys

hangman :: IO ()
hangman = do
  putStrLn "enter secret word"
  secret <- getLine
  play secret
 where
  play :: String -> IO ()
  play secret = do
    putStrLn "enter your guess"
    guess <- getLine
    if guess == secret
      then putStrLn ("Got it: " ++ secret)
      else do
        putStrLn (intersect guess secret)
        play secret

type Picture = [String]
type World = (Picture, Int)
type Move = (Int, Int)

empty::[String]->Bool
empty = all (== [])

putPict::Picture->IO()
putPict ss = go 1 ss
    where go _ [] = putStrLn ""
          go i (s:ss) = do
            putStrLn ((show i) ++ ". " ++ s)
            go (succ i) ss

-- show 10 ++ " ." ++ "some"
-- ss=[1,3,4]
-- ss !! 2

apply::Move->Picture->Picture
apply (r, c) ss = take (r - 1) ss ++ [take (c - 1) (ss !! (r - 1))] ++ drop r ss

nim :: World -> IO ()
nim (ss, player) = do
  putPict ss
  move <- getMove
  let ss' = apply move ss in
    if empty ss'
      then putStrLn ("Player " ++ (show player) ++ " won")
      else nim (ss', (next player))
 where
  next 1 = 2
  next 2 = 1
  getMove :: IO (Int, Int)
  getMove = do
    putStrLn "enter row"
    r <- getLine
    putStrLn "enter column"
    c <- getLine
    return (read r :: Int, read c :: Int)

playNim::IO()
playNim = nim (ss,1)
  where ss = ["*****","****","***","**","*"]




strlen :: IO Int
strlen = do
  putStr "enter text:"
  xs <- getLine
  putStr "text length is "
  putStr (show (length xs))
  putStrLn " characters"
  return $ length xs

act :: IO (Char, Char)
act = do
  x <- getChar
  _ <- getChar
  y <- getChar
  return (x, y)


