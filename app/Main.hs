module Main where

import Data.Array (Array, array, bounds, (!))
import qualified Data.Array as Arr
import Data.Ix
import Lib

main :: IO ()
main = someFunc

data Cell = River | RiverHead | Forest | Thicket | Empty

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

worldmapArr (WorldMap arr) = arr

buildWorldmap r c = worldmap ((0, 0), (r -1, c -1)) ([((i, j), Empty) | i <- [0 .. r -1], j <- [0 .. c -1]])

fillWithThickets :: WorldMap Cell -> WorldMap Cell
fillWithThickets (WorldMap arr) = WorldMap $ fmap f arr
  where
    f Empty = Thicket
    f a = a


(//) :: WorldMap e -> [((Integer, Integer), e)] -> WorldMap e
(//) (WorldMap arr) c = WorldMap $ arr Arr.// c

worldmap3x3 = buildWorldmap 3 3

worldmap4x4 = buildWorldmap 4 4

speedBoostPoint (WorldMap arr) pos = if n == 0 then val else 2 * n * val
  where
    val = f (arr ! pos)
    n = sum $ map (fromEnum . isRiver) $ crossNeighbour (WorldMap arr) pos
    f Thicket = 2
    f Forest = 1
    f _ = 0

speedBoost (WorldMap arr) = sum $ map (speedBoostPoint (WorldMap arr)) $ range $ bounds arr

isRiver (Just River) = True
isRiver (Just RiverHead) = True
isRiver _ = False

crossNeighbour (WorldMap arr) (i, j) = map mbval [(i + 1, j), (i, j + 1), (i -1, j), (i, j -1)]
  where
    valid pos = inRange (bounds arr) pos
    mbval pos = if valid pos then Just (arr ! pos) else Nothing

testmap1 =
  buildWorldmap 4 4
    // [ ((1, 1), Thicket)
       ]

testmap2 =
  buildWorldmap 4 4
    // [ ((1, 1), Thicket),
         ((0, 1), RiverHead)
       ]

testmap3 =
  buildWorldmap 4 4
    // [ ((1, 1), Thicket),
         ((0, 1), River),
         ((0, 2), RiverHead)
       ]

testmap4 =
  buildWorldmap 4 4
    // [ ((1, 1), Thicket),
         ((0, 1), River),
         ((0, 2), River),
         ((1, 2), RiverHead)
       ]

testmap5 =
  buildWorldmap 4 4
    // [ ((1, 1), Thicket),
         ((0, 1), River),
         ((0, 2), River),
         ((1, 2), River),
         ((2, 2), RiverHead)
       ]

testmap6 =
  buildWorldmap 4 4
    // [ ((1, 1), Thicket),
         ((0, 1), River),
         ((0, 2), River),
         ((1, 2), River),
         ((2, 2), River),
         ((2, 1), RiverHead)
       ]

testmap7 =
  buildWorldmap 4 4
    // [ ((1, 1), Thicket),
         ((0, 1), River),
         ((0, 2), River),
         ((1, 2), River),
         ((2, 2), River),
         ((2, 1), River),
         ((2, 0), RiverHead)
       ]

testmap8 =
  buildWorldmap 4 4
    // [ ((1, 1), Thicket),
         ((0, 1), River),
         ((0, 2), River),
         ((1, 2), River),
         ((2, 2), River),
         ((2, 1), River),
         ((2, 0), River),
         ((1, 0), RiverHead)
       ]

testmap9 =
  buildWorldmap 4 4
    // [ ((1, 1), Thicket),
         ((0, 1), River),
         ((0, 2), River),
         ((1, 2), River),
         ((2, 2), River),
         ((2, 1), River),
         ((2, 0), River),
         ((1, 0), River),
         ((0, 0), RiverHead)
       ]