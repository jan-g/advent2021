{-# Language TupleSections #-}

module Day9 where

import Data.Function ((&))
import Data.List.Split
import Data.List as L
import Data.Array as A
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe (catMaybes, isJust, fromJust, fromMaybe)
import Text.ParserCombinators.ReadP as P
import Numeric (readInt)
import Data.Bits ((.&.), (.|.))

import Debug.Trace (trace)

import Lib


{-
--- Day 9: Smoke Basin ---

These caves seem to be lava tubes. Parts are even still volcanically active; small hydrothermal vents release smoke into the caves that slowly settles like rain.

If you can model how the smoke flows through the caves, you might be able to avoid it and be that much safer. The submarine generates a heightmap of the floor of the nearby caves for you (your puzzle input).

Smoke flows to the lowest point of the area it's in. For example, consider the following heightmap:

2199943210
3987894921
9856789892
8767896789
9899965678

Each number corresponds to the height of a particular location, where 9 is the highest and 0 is the lowest a location can be.

Your first goal is to find the low points - the locations that are lower than any of its adjacent locations. Most locations have four adjacent locations (up, down, left, and right); locations on the edge or corner of the map have three or two adjacent locations, respectively. (Diagonal locations do not count as adjacent.)

In the above example, there are four low points, all highlighted: two are in the first row (a 1 and a 0), one is in the third row (a 5), and one is in the bottom row (also a 5). All other locations on the heightmap have some lower adjacent location, and so are not low points.

The risk level of a low point is 1 plus its height. In the above example, the risk levels of the low points are 2, 1, 6, and 6. The sum of the risk levels of all low points in the heightmap is therefore 15.

Find all of the low points on your heightmap. What is the sum of the risk levels of all low points on your heightmap?

To begin, get your puzzle input.
-}

parse ls = ls
         & loadMapWith (\(x,y) c -> read [c] :: Integer)

day9 ls =
  let m = parse ls
  in m & Map.filterWithKey (\(x,y) d -> all (\(a,b) -> case m Map.!? (a,b) of Just e -> d < e; Nothing -> True) (neighbours orthogonalMoves (x,y)))
       & Map.toList
       & map snd
       & map (+ 1)
       & sum

{-
--- Part Two ---

Next, you need to find the largest basins so you know what areas are most important to avoid.

A basin is all locations that eventually flow downward to a single low point. Therefore, every low point has a basin, although some basins are very small. Locations of height 9 do not count as being in any basin, and all other locations will always be part of exactly one basin.

The size of a basin is the number of locations within the basin, including the low point. The example above has four basins.

The top-left basin, size 3:

2199943210
3987894921
9856789892
8767896789
9899965678

The top-right basin, size 9:

2199943210
3987894921
9856789892
8767896789
9899965678

The middle basin, size 14:

2199943210
3987894921
9856789892
8767896789
9899965678

The bottom-right basin, size 9:

2199943210
3987894921
9856789892
8767896789
9899965678

Find the three largest basins and multiply their sizes together. In the above example, this is 9 * 14 * 9 = 1134.

What do you get if you multiply together the sizes of the three largest basins?
-}

floodFill :: Map.Map (Integer, Integer) Integer -> (Integer, Integer) -> Set.Set (Integer, Integer)
floodFill m (x0,y0) =
  let r = flood nextMoves
          (const False)
          id
          (1, (x0,y0))
  in case r of Left x -> x; Right x -> x
  where
    nextMoves :: (Integer, (Integer, Integer)) -> Set.Set (Integer, (Integer, Integer))
    nextMoves (_, (x, y)) = neighbours orthogonalMoves (x,y)
                          & filter (\(a,b) -> case m Map.!? (a,b) of
                                                Nothing -> False
                                                Just d -> d < 9)
                          & map (1,)
                          & Set.fromList

allRegions m =
  let lowPoints = m
                & Map.filterWithKey (\(x,y) d -> all (\(a,b) -> case m Map.!? (a,b) of Just e -> d < e; Nothing -> True) (neighbours orthogonalMoves (x,y)))
                & Map.keys
  in [floodFill m (x0,y0) | (x0,y0) <- lowPoints] & Set.fromList

day9b ls =
  let m = parse ls
      rs = allRegions m
  in  Set.toList rs
    & map Set.size
    & L.sort
    & reverse
    & take 3
    & product
