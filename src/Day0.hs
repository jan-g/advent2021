module Day0 where

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
                               target: 30
                              /
                             V
   ( * )---( 8 )---( - )---( 1 )
     |       |       |       |
   ( 4 )---( * )---( 11)---( * )
     |       |       |       |
   ( + )---( 4 )---( - )---( 18)
     |       |       |       |
   ( 22)---( - )---( 9 )---( * )
     ^
    /
  start
-}

data Op = Mult | Add | Sub deriving (Show, Eq, Ord)
data Content = Value Int | Op Op deriving (Show, Eq)
data Orb = Number Int | Glowing Int Op deriving (Show, Eq, Ord)

type Coord = (Integer, Integer)

maze = Map.fromList [
          ((0, 0), Op Mult),  ((1, 0), Value 8), ((2, 0), Op Sub),   ((3, 0), Value 1),
          ((0, 1), Value 4),  ((1, 1), Op Mult), ((2, 1), Value 11), ((3, 1), Op Mult),
          ((0, 2), Op Add),   ((1, 2), Value 4), ((2, 2), Op Sub),   ((3, 2), Value 18),
                              ((1, 3), Op Sub),  ((2, 3), Value 9),  ((3, 3), Op Mult)
       ]

sought = 30
starting = 22
neighs (x, y) = neighbours orthogonalMoves (x, y) & filter (\c -> Map.member c maze)

next (c, (coord, orb, path)) =
  let ns = neighs coord
  in if coord == (3,0) then Set.empty else
     if c > 16 then Set.empty else
     Set.fromList $ do
      n <- ns
      let room = maze Map.! n
          orb' = updateOrb orb room
          path' = path ++ [n]
      return (c + 1, (n, orb', path'))

updateOrb (Number n) (Op o) = Glowing n o
updateOrb (Glowing n Mult) (Value v) = Number $ n * v
updateOrb (Glowing n Add) (Value v) = Number $ n + v
updateOrb (Glowing n Sub) (Value v) = Number $ n - v

finished (c, (coord, orb, path)) =
  (coord == (3, 0) && orb == (Number sought))

day1 ls =
  let Just (m, (_, _, p)) = bfs next finished id (0, ((0, 3), Number 22, [(0, 3)]))
  in (m, p, map (\c -> case Map.lookup c maze of
                        Nothing -> "22"
                        Just (Op o) -> (show o)
                        Just (Value v) -> (show v)) p & intercalate " ")

{-
-}

day1b ls = "hello world"
