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


strlen :: IO (Int)
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


