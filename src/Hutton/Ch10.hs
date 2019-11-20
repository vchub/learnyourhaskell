{-# OPTIONS_GHC -Wall #-}

module Hutton.Ch10
  ( strlen
  , intersect
  , hangman
  , act
  , rmdups
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
  putStr "Player: "
  putStrLn (show player)

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

-- ==============================
-- Game of life

cls::IO()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

writeat:: Pos->String->IO()
writeat p xs = do goto p
                  putStr xs

goto::Pos->IO()
goto (x,y) = putStr ("\ESC[" ++ (show y)++";"++(show x)++"H")

width :: Int
width = 10

height :: Int
height = 10

type Board = [Pos]

glider :: Board
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]

showcells::Board->IO()
showcells b = sequence_ [writeat p "O" | p<-b]

wait::Int->IO()
wait n = sequence_ [return () | _ <-[1..n]]

isEmpty::Board->Pos->Bool
isEmpty b p= not (isAlive b p)

isAlive::Board->Pos->Bool
isAlive b p = elem p b

rmdups::Eq a=>[a]->[a]
rmdups []     = []
rmdups (x:xs) = x: filter (/= x) (rmdups xs)

neighbors::Pos->[Pos]
neighbors (x,y) = map wrap [(x-1, y-1),(x,y-1),
                           (x+1,y-1), (x-1,y),
                           (x+1, y), (x-1,y+1),
                           (x,y+1),(x+1,y+1)]

wrap::Pos->Pos
wrap (x,y) = (((x-1) `mod` width) + 1,
             ((y-1) `mod` height) + 1)

liveneighbors::Board->Pos->Int
liveneighbors b = length.filter (isAlive b).neighbors

births::Board->[Pos]
births b = [p | p<-rmdups (concat (map neighbors b)), isEmpty b p, liveneighbors b p == 3]

survivors::Board->[Pos]
survivors b = [p | p<-b, elem (liveneighbors b p) [2,3] ]

nextgen::Board->Board
nextgen b = survivors b ++ births b

life::Board -> IO()
life b = do
  cls
  showcells b
  wait 500000
  life (nextgen b)







