{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module CIS194.MyList where

-- import           CIS194.Homework2
import           CodeWorld

data MyList a = Empty | Entry a (MyList a)

fstL :: MyList a -> a
fstL Empty       = error "empty MyList"
fstL (Entry x _) = x

fldL :: (b -> a -> b) -> b -> MyList a -> b
fldL _ b Empty        = b
fldL f b (Entry x xs) = fldL f (f b x) xs

toList :: MyList a -> [a]
toList = fldL (\acc x -> x : acc) []

fromList :: [a] -> MyList a
fromList = foldl (\acc x -> (Entry x acc)) Empty

mapL :: (a -> b) -> MyList a -> MyList b
mapL f = fldL (\acc x -> (Entry (f x) acc)) Empty


data Tile = Wall| Ground| Storage|Box|Blank
data Direction = R | U | L | D
data Coord = C Integer Integer
data Interaction s = Interaction s (Event->s->s) (s->Picture)
type Boxes = MyList Coord

drawTile :: Tile -> Picture
drawTile Wall    = colored grey (solidRectangle 1 1)
drawTile Ground  = colored yellow (solidRectangle 1 1)
drawTile Storage = solidCircle 0.3 & drawTile Ground
drawTile Box     = colored brown (solidRectangle 1 1)
drawTile Blank   = blank

drawAt :: Coord -> Picture -> Picture
drawAt (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x + 1) y
adjacentCoord U (C x y) = C x (y + 1)
adjacentCoord L (C x y) = C (x - 1) y
adjacentCoord D (C x y) = C x (y - 1)

boxes0 :: Boxes
boxes0 = fromList [(C (-2) 1), (C (-1) 1)]

movingBoxes :: Boxes -> Interaction Boxes
movingBoxes Empty = Interaction Empty (\_ s -> s) (\_ -> blank)
movingBoxes xs    = Interaction xs handle draw
 where
  move :: Direction -> Boxes -> Boxes
  move dir s = mapL (\c -> adjacentCoord dir c) s
  handle (KeyPress "Right") s = move R s
  handle (KeyPress "Left" ) s = move L s
  handle (KeyPress "Up"   ) s = move U s
  handle (KeyPress "Down" ) s = move D s
  handle _                  s = s
  draw s = fldL (\acc c -> drawAt c $ drawTile Box & acc) (drawTile Blank) s

runInteraction :: Interaction s -> IO ()
runInteraction (Interaction s0 handle draw) = activityOf s0 handle draw


main :: IO ()
main = runInteraction $ movingBoxes boxes0


