module Main where

import Data.Array
import Data.Ix
import Lib

main :: IO ()
main = someFunc

data Cell = River | RiverHead | Thicket | Empty

instance Show Cell where
  show River = " r "
  show RiverHead = " R "
  show Thicket = " t "
  show Empty = " _ "

newtype WorldMap e = WorldMap (Array (Integer, Integer) e)

instance Show e => Show (WorldMap e) where
  show (WorldMap arr) = concat [line i ++ ['\n'] | i <- [r0 .. r1]]
    where
      ((r0, c0), (r1, c1)) = bounds arr
      line n = concatMap show [arr ! (n, i) | i <- [c0 .. c1]]

worldmap :: ((Integer, Integer), (Integer, Integer)) -> [((Integer, Integer), e)] -> WorldMap e
worldmap i e = WorldMap $ array i e

worldmap3x3 = buildWorldmap 3 3

worldmap4x4 = buildWorldmap 4 4

buildWorldmap r c = worldmap ((0, 0), (r -1, c -1)) ([((i, j), Empty) | i <- [0 .. r -1], j <- [0 .. c -1]])

fillWithThickets (WorldMap arr) = WorldMap $ fmap f arr
  where
    f Empty = Thicket
    f a = a