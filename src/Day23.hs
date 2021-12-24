module Day23 where

import Data.Function ((&))
import Data.List.Split
import Data.List as L
import Data.Array as A
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe (catMaybes, isJust, fromJust, fromMaybe, mapMaybe)
import Text.ParserCombinators.ReadP as P
import Numeric (readInt)
import Data.Bits ((.&.), (.|.))

import Debug.Trace (trace)

import Lib
import Control.Monad (guard)


{-
--- Day 23: Amphipod ---

A group of amphipods notice your fancy submarine and flag you down. "With such an impressive shell," one amphipod says, "surely you can help us with a question that has stumped our best scientists."

They go on to explain that a group of timid, stubborn amphipods live in a nearby burrow. Four types of amphipods live there: Amber (A), Bronze (B), Copper (C), and Desert (D). They live in a burrow that consists of a hallway and four side rooms. The side rooms are initially full of amphipods, and the hallway is initially empty.

They give you a diagram of the situation (your puzzle input), including locations of each amphipod (A, B, C, or D, each of which is occupying an otherwise open space), walls (#), and open space (.).

For example:

#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########

The amphipods would like a method to organize every amphipod into side rooms so that each side room contains one type of amphipod and the types are sorted A-D going left to right, like this:

#############
#...........#
###A#B#C#D###
  #A#B#C#D#
  #########

Amphipods can move up, down, left, or right so long as they are moving into an unoccupied open space. Each type of amphipod requires a different amount of energy to move one step: Amber amphipods require 1 energy per step, Bronze amphipods require 10 energy, Copper amphipods require 100, and Desert ones require 1000. The amphipods would like you to find a way to organize the amphipods that requires the least total energy.

However, because they are timid and stubborn, the amphipods have some extra rules:

    Amphipods will never stop on the space immediately outside any room. They can move into that space so long as they immediately continue moving. (Specifically, this refers to the four open spaces in the hallway that are directly above an amphipod starting position.)
    Amphipods will never move from the hallway into a room unless that room is their destination room and that room contains no amphipods which do not also have that room as their own destination. If an amphipod's starting room is not its destination room, it can stay in that room until it leaves the room. (For example, an Amber amphipod will not move from the hallway into the right three rooms, and will only move into the leftmost room if that room is empty or if it only contains other Amber amphipods.)
    Once an amphipod stops moving in the hallway, it will stay in that spot until it can move into a room. (That is, once any amphipod starts moving, any other amphipods currently in the hallway are locked in place and will not move again until they can move fully into a room.)

In the above example, the amphipods can be organized using a minimum of 12521 energy. One way to do this is shown below.

Starting configuration:

#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########

One Bronze amphipod moves into the hallway, taking 4 steps and using 40 energy:

#############
#...B.......#
###B#C#.#D###
  #A#D#C#A#
  #########

The only Copper amphipod not in its side room moves there, taking 4 steps and using 400 energy:

#############
#...B.......#
###B#.#C#D###
  #A#D#C#A#
  #########

A Desert amphipod moves out of the way, taking 3 steps and using 3000 energy, and then the Bronze amphipod takes its place, taking 3 steps and using 30 energy:

#############
#.....D.....#
###B#.#C#D###
  #A#B#C#A#
  #########

The leftmost Bronze amphipod moves to its room using 40 energy:

#############
#.....D.....#
###.#B#C#D###
  #A#B#C#A#
  #########

Both amphipods in the rightmost room move into the hallway, using 2003 energy in total:

#############
#.....D.D.A.#
###.#B#C#.###
  #A#B#C#.#
  #########

Both Desert amphipods move into the rightmost room using 7000 energy:

#############
#.........A.#
###.#B#C#D###
  #A#B#C#D#
  #########

Finally, the last Amber amphipod moves into its room, using 8 energy:

#############
#...........#
###A#B#C#D###
  #A#B#C#D#
  #########

What is the least energy required to organize the amphipods?

To begin, get your puzzle input.
-}

grid0 = "#############\n\
        \#12!3!4!5!67#\n\
        \###a#b#c#d###\n\
        \  #A#B#C#D#\n\
        \  #########" & lines & loadMap

moves =
  let m =
        ["a!21", "a!2", "a!3", "a!3!4", "a!3!4!5", "a!3!4!5!6", "a!3!4!5!67"
        ,"Aa!21", "Aa!2", "Aa!3", "Aa!3!4", "Aa!3!4!5", "Aa!3!4!5!6", "Aa!3!4!5!67"
        ,"b!3!21", "b!3!2", "b!3", "b!4", "b!4!5", "b!4!5!6", "b!4!5!67"
        ,"Bb!3!21", "Bb!3!2", "Bb!3", "Bb!4", "Bb!4!5", "Bb!4!5!6", "Bb!4!5!67"
        ,"c!4!3!21", "c!4!3!2", "c!4!3", "c!4", "c!5", "c!5!6", "c!5!67"
        ,"Cc!4!3!21", "Cc!4!3!2", "Cc!4!3", "Cc!4", "Cc!5", "Cc!5!6", "Cc!5167"
        ,"d!5!4!3!21", "d!5!4!3!2", "d!5!4!3", "d!5!4", "d!5", "d!6", "d!67"
        ,"Dd!5!4!3!21", "Dd!5!4!3!2", "Dd!5!4!3", "Dd!5!4", "Dd!5", "Dd!6", "Dd!67"
        ]
  in
    map (\s -> Move { from=head s, to=last s, via=Set.fromList (tail s), cost=length s - 1}) m ++
    map (\s -> Move { from=last s, to=head s, via=Set.fromList (init s), cost=length s - 1}) m

type Pos = Char
type Amphi = Char
type State = Map.Map Pos Amphi

target = Map.fromList ("aAbBcCdD" `zip` "AABBCCDD")

finished :: State -> Bool
finished s = s == target

amphiCost :: Amphi -> Int
amphiCost 'A' = 1
amphiCost 'B' = 10
amphiCost 'C' = 100
amphiCost 'D' = 1000

allMoves :: State -> [(State, Int)]
allMoves s = do
  m <- moves
  let p = s Map.!? (from m)
  {- ensure there's something there -}
  guard $ isJust p
  {- ensure there's nothing in the way -}
  guard $ Set.filter (`Map.member` s) (via m) & Set.null
  {- Amphipods will never move from the hallway into a room unless that room is their destination room and that room
     contains no amphipods which do not also have that room as their own destination. If an amphipod's starting room is
     not its destination room, it can stay in that room until it leaves the room. -}
  guard $ (not . isAlpha $ to m) ||
          (fromJust p == to m) ||
          (toLower (fromJust p) == to m &&
           s Map.!? (toUpper $ to m) == p)
  {- if this room is finished, don't move away from it -}
  guard $ (isAlpha $ to m) ||
          (fromJust p /= toUpper (from m)) ||     -- moving out of a room not ours
          (isLower (from m) && s Map.!? (toUpper $ from m) /= p)   -- catch moves from the top half of a correctly-filled room
  {- if the bottom half of the room is empty, don't occupy the top half -}
  guard $ (not . isAlpha $ to m) ||
          (isUpper $ to m) ||
          (s Map.!? (toUpper $ to m) == p)
  return $ (Map.delete (from m) s & Map.insert (to m) (fromJust p)
           ,cost m * amphiCost (fromJust p))

data Move = Move { from :: Pos, to :: Pos, via :: Set.Set Pos, cost :: Int } deriving (Show, Eq)


parse ls =
  let
    m = loadMap ls
    pos = m & Map.filter isAlpha & Map.mapKeysWith const (grid0 Map.!)
  in pos

day23 ls =
  let
    state0 = parse ls
  in
    bfs nextStates satisfying summariseState startState
  where
  nextStates :: (Int, State) -> Set.Set (Int, State)
  nextStates (c, s) = allMoves s
                    & map (\(s',c') -> (c+c', s'))
                    & Set.fromList
  satisfying :: (Int, State) -> Bool
  satisfying (_, s) = finished s
  summariseState :: State -> State
  summariseState = id
  startState :: (Int, State)
  startState = (0, parse ls)


{-
--- Part Two ---

As you prepare to give the amphipods your solution, you notice that the diagram they handed you was actually folded up. As you unfold it, you discover an extra part of the diagram.

Between the first and second lines of text that contain amphipod starting positions, insert the following lines:

  #D#C#B#A#
  #D#B#A#C#

So, the above example now becomes:

    #############
    #...........#
    ###B#C#B#D###
      #D#C#B#A#
      #D#B#A#C#
      #A#D#C#A#
      #########

The amphipods still want to be organized into rooms similar to before:

    #############
    #...........#
    ###A#B#C#D###
      #A#B#C#D#
      #A#B#C#D#
      #A#B#C#D#
      #########

In this updated example, the least energy required to organize these amphipods is 44169:

    #############
    #...........#
    ###B#C#B#D###
      #D#C#B#A#
      #D#B#A#C#
      #A#D#C#A#
      #########

    #############
    #..........D#
    ###B#C#B#.###
      #D#C#B#A#
      #D#B#A#C#
      #A#D#C#A#
      #########

    #############
    #A.........D#
    ###B#C#B#.###
      #D#C#B#.#
      #D#B#A#C#
      #A#D#C#A#
      #########

    #############
    #A........BD#
    ###B#C#.#.###
      #D#C#B#.#
      #D#B#A#C#
      #A#D#C#A#
      #########

    #############
    #A......B.BD#
    ###B#C#.#.###
      #D#C#.#.#
      #D#B#A#C#
      #A#D#C#A#
      #########

    #############
    #AA.....B.BD#
    ###B#C#.#.###
      #D#C#.#.#
      #D#B#.#C#
      #A#D#C#A#
      #########

    #############
    #AA.....B.BD#
    ###B#.#.#.###
      #D#C#.#.#
      #D#B#C#C#
      #A#D#C#A#
      #########

    #############
    #AA.....B.BD#
    ###B#.#.#.###
      #D#.#C#.#
      #D#B#C#C#
      #A#D#C#A#
      #########

    #############
    #AA...B.B.BD#
    ###B#.#.#.###
      #D#.#C#.#
      #D#.#C#C#
      #A#D#C#A#
      #########

    #############
    #AA.D.B.B.BD#
    ###B#.#.#.###
      #D#.#C#.#
      #D#.#C#C#
      #A#.#C#A#
      #########

    #############
    #AA.D...B.BD#
    ###B#.#.#.###
      #D#.#C#.#
      #D#.#C#C#
      #A#B#C#A#
      #########

    #############
    #AA.D.....BD#
    ###B#.#.#.###
      #D#.#C#.#
      #D#B#C#C#
      #A#B#C#A#
      #########

    #############
    #AA.D......D#
    ###B#.#.#.###
      #D#B#C#.#
      #D#B#C#C#
      #A#B#C#A#
      #########

    #############
    #AA.D......D#
    ###B#.#C#.###
      #D#B#C#.#
      #D#B#C#.#
      #A#B#C#A#
      #########

    #############
    #AA.D.....AD#
    ###B#.#C#.###
      #D#B#C#.#
      #D#B#C#.#
      #A#B#C#.#
      #########

    #############
    #AA.......AD#
    ###B#.#C#.###
      #D#B#C#.#
      #D#B#C#.#
      #A#B#C#D#
      #########

    #############
    #AA.......AD#
    ###.#B#C#.###
      #D#B#C#.#
      #D#B#C#.#
      #A#B#C#D#
      #########

    #############
    #AA.......AD#
    ###.#B#C#.###
      #.#B#C#.#
      #D#B#C#D#
      #A#B#C#D#
      #########

    #############
    #AA.D.....AD#
    ###.#B#C#.###
      #.#B#C#.#
      #.#B#C#D#
      #A#B#C#D#
      #########

    #############
    #A..D.....AD#
    ###.#B#C#.###
      #.#B#C#.#
      #A#B#C#D#
      #A#B#C#D#
      #########

    #############
    #...D.....AD#
    ###.#B#C#.###
      #A#B#C#.#
      #A#B#C#D#
      #A#B#C#D#
      #########

    #############
    #.........AD#
    ###.#B#C#.###
      #A#B#C#D#
      #A#B#C#D#
      #A#B#C#D#
      #########

    #############
    #..........D#
    ###A#B#C#.###
      #A#B#C#D#
      #A#B#C#D#
      #A#B#C#D#
      #########

    #############
    #...........#
    ###A#B#C#D###
      #A#B#C#D#
      #A#B#C#D#
      #A#B#C#D#
      #########

    Using the initial configuration from the full diagram, what is the least energy required to organize the amphipods?
-}


type Pos' = (Integer, Integer)
data Move' = Move' { from' :: Pos'
                   , to' :: Pos'
                   , via' :: Set.Set Pos'
                   , isColumn :: Maybe Amphi
                   , below :: Set.Set Pos'   -- other cells in this column, below this one
                   , cost' :: Int  -- how many moves
                   } deriving (Show, Eq)


grid1 = "#############\n\
        \#12!3!4!5!67#\n\
        \###a#b#c#d###" & lines & loadMap

findCell :: Char -> Maybe Pos'
findCell c = case Map.filter (== c) grid1 & Map.toList & map fst of
              [p] -> Just p
              _ -> Nothing

add (a,b) (c,d) = (a+c, b+d)

{- generate all moves -}
moves' = m0 ++ m1 ++ m2 ++ m3 ++ m0' ++ m1' ++ m2' ++ m3'
  where
  m = ["a!21", "a!2", "a!3", "a!3!4", "a!3!4!5", "a!3!4!5!6", "a!3!4!5!67"
      ,"b!3!21", "b!3!2", "b!3", "b!4", "b!4!5", "b!4!5!6", "b!4!5!67"
      ,"c!4!3!21", "c!4!3!2", "c!4!3", "c!4", "c!5", "c!5!6", "c!5!67"
      ,"d!5!4!3!21", "d!5!4!3!2", "d!5!4!3", "d!5!4", "d!5", "d!6", "d!67"
      ]
  m0 :: [Move']
  m0 = map (\s -> Move' { from'=head s & findCell & fromJust
                        , to'=last s & findCell & fromJust
                        , via'=Set.fromList $ mapMaybe findCell (tail s)
                        , cost'=length s - 1
                        , isColumn=Nothing
                        , below=Set.empty
                        }) m
  m1 :: [Move']
  m1 = map (\m -> m { from'=from' m `add` (0,1)
                    , via'=from' m `Set.insert` via' m
                    , cost'=cost' m + 1}) m0
  m2 :: [Move']
  m2 = map (\m -> m { from'=from' m `add` (0,1)
                    , via'=from' m `Set.insert` via' m
                    , cost'=cost' m + 1}) m1
  m3 :: [Move']
  m3 = map (\m -> m { from'=from' m `add` (0,1)
                    , via'=from' m `Set.insert` via' m
                    , cost'=cost' m + 1}) m2
  m0' :: [Move']
  m0' = map (\s -> let at = last s & findCell & fromJust
                       go = head s & findCell & fromJust
                   in
                   Move' { from'=at
                         , to'=go
                         , via'=Set.fromList $ mapMaybe findCell (init s)
                         , cost'=length s - 1
                         , isColumn=Just (toUpper $ head s)
                         , below=Set.fromList [go `add` (0,1), go `add` (0,2), go `add` (0,3)]
                         }) m
  m1' :: [Move']
  m1' = map (\m -> m { to'=to' m `add` (0,1)
                     , via'=to' m `add` (0,1) `Set.insert` via' m
                     , cost'=cost' m + 1
                     , below=Set.delete (to' m `add` (0,1)) (below m)}) m0'
  m2' :: [Move']
  m2' = map (\m -> m { to'=to' m `add` (0,1)
                     , via'=to' m `add` (0,1) `Set.insert` via' m
                     , cost'=cost' m + 1
                     , below=Set.delete (to' m `add` (0,1)) (below m)}) m1'
  m3' :: [Move']
  m3' = map (\m -> m { to'=to' m `add` (0,1)
                     , via'=to' m `add` (0,1) `Set.insert` via' m
                     , cost'=cost' m + 1
                     , below=Set.delete (to' m `add` (0,1)) (below m)}) m2'


type State' = Map.Map Pos' Amphi

target' = Map.fromList $ m0 ++ m1 ++ m2 ++ m3
  where
  m0 :: [(Pos', Amphi)]
  m0 = do
    (p,c) <- ("abcd" `zip` "ABCD")
    return $ (p & findCell & fromJust, c)
  m1 :: [(Pos', Amphi)]
  m1 = map (\(p,c) -> (p `add` (0,1), c)) m0
  m2 :: [(Pos', Amphi)]
  m2 = map (\(p,c) -> (p `add` (0,1), c)) m1
  m3 :: [(Pos', Amphi)]
  m3 = map (\(p,c) -> (p `add` (0,1), c)) m2

finished' :: State' -> Bool
finished' s = s == target'


allMoves' :: State' -> [(State', Int)]
allMoves' s = do
  m <- moves'
  let p = s Map.!? (from' m)
  {- ensure there's something there -}
  guard $ isJust p
  {- ensure there's nothing in the way -}
  guard $ Set.filter (`Map.member` s) (via' m) & Set.null
  {- Amphipods will never move from the hallway into a room unless that room is their destination room and that room
     contains no amphipods which do not also have that room as their own destination. If an amphipod's starting room is
     not its destination room, it can stay in that room until it leaves the room. -}
  guard $ (not . isJust $ isColumn m) ||  -- this is a move into a hallway; it's okay
          (p == isColumn m &&             -- it's the right room
           (Set.filter (\pos -> s Map.!? pos /= p) (below m) & Set.null))  -- everything below it is full of the right thing
  {- if this room is finished, don't move away from it -}
  guard $ (isJust $ isColumn m) ||        -- this is a move into a room, don't care
          (p /= isColumn m) ||            -- moving out of a room not ours
          (Set.filter (\pos -> s Map.!? pos /= p) (below m)   -- consider anything below us that's empty or not us
           & not . Set.null)  -- there's something below us that's alien; we can move away
                              -- ie, prevent moves from the top half of a correctly-filled room
  return $ (Map.delete (from' m) s & Map.insert (to' m) (fromJust p)
           ,cost' m * amphiCost (fromJust p))


day23b ls =
--  trace ("all moves are " ++ show moves' ++ " of length " ++ (show $ length moves')) $
--  trace ("start state: " ++ show startState) $
--  trace ("target state: " ++ show target') $
  bfs nextStates satisfying summariseState (startState' ls)
  -- aStar nextStates satisfying cost heuristic' id (startState' ls)
  where
  nextStates :: (Int, State') -> Set.Set (Int, State')
  nextStates (c, s) = allMoves' s
                    & map (\(s',c') -> (c+c', s'))
                    & Set.fromList
  satisfying :: (Int, State') -> Bool
  satisfying (_, s) = finished' s
  summariseState :: State' -> State'
  summariseState = id
  cost = fst
  
startState' :: [String] -> (Int, State')
startState' ls = (0, parse)
  where
  ls' = (take 3 ls) ++ ["  #D#C#B#A#", "  #D#B#A#C#"] ++ (drop 3 ls)
  parse = loadMap ls'
        & Map.filter isAlpha

heuristic' :: (Int, State') -> Int
heuristic' (_, s) = (cost 3 'A') + 10 * (cost 5 'B') + 100 * (cost 7 'C') + 1000 * (cost 9 'D')
  where
  cost = cost1
  cost0 x c =
    let cs = [s Map.!? (x,y) == Just c | y <- [2..5]]
    in case cs of
      [_, _, _, False] -> 10
      [_, _, False, True] -> 6
      [_, False, True, True] -> 3
      [False, True, True, True] -> 1
      [True, True, True, True] -> 0
  cost1 x c = sum $ do
    ((p, _), a) <- Map.toList s
    guard $ a == c
    guard $ p /= x
    return $ fromInteger $ abs (x - p)
