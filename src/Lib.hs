module Lib
    ( loadLines
    , makeArray
    , natParser
    , intParser
    , binParser
    , readBin
    , quickParse
    , drawMapWith
    , mapReverse
    , mapReverseAll
    , narrowCandidateMap
    , (<<<<)
    , (>>>>)
    , (<<>>)
    , (<<!!)
    , loadMap
    , loadMapWith
    , boundMap, offsetMap, normaliseMap
    , rotateLeftMap, rotateRightMap, rotate180Map
    , flipXMap, flipYMap
    , subMap, diceMap, undiceMap
    , wordParser
    , euc
    , solveDiophantine
    , combineDiophantine
    , search, bfs, flood, floodFill
    , aStar
    , orthogonalMoves, kingsMoves, neighbours, neighboursIn
    , swap
    , squareRoot
    , hexToBin
    ) where

import Data.Array
import Data.Char
import Text.ParserCombinators.ReadP
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Function ((&))
import qualified Data.Heap as H

loadLines fn = do
  contents <- readFile fn
  return (lines contents)

makeArray :: [a] -> Array Int a
makeArray ns = listArray (0, length ns - 1) ns

natParser :: ReadP Integer
natParser = do
  digits <- munch1 isDigit
  return $ read digits

intParser :: ReadP Integer
intParser = do
  (do
    char '-'
    i <- natParser
    return $ -i) <++ natParser

binParser :: ReadP Integer
binParser = do
  bits <- munch1 isBit
  return $ readBin bits
  where
    isBit '0' = True
    isBit '1' = True
    isBit _ = False

readBin :: String -> Integer
readBin = foldl (\acc d -> acc * 2 + bit d) 0
  where
    bit '0' = 0
    bit '1' = 1

quickParse :: ReadP t -> String -> Maybe t
quickParse parser s =
  case readP_to_S parser s of
    [] -> Nothing
    [(m, "")] -> Just m
    _ -> Nothing

drawMapWith :: (Ord a, Enum a) => ((a, a) -> Maybe b -> c) -> Map.Map (a, a) b -> [[c]]
drawMapWith f m =
  let coords = Map.keysSet m
      xs = Set.map fst coords
      ys = Set.map snd coords
      (x0, x1) = (Set.findMin xs, Set.findMax xs)
      (y0, y1) = (Set.findMin ys, Set.findMax ys)
  in  [[f (x, y) (Map.lookup (x, y) m) | x <- [x0..x1]] | y <- [y0..y1]]

mapReverse :: (Ord v, Ord k) => Map.Map k v -> Map.Map v (Set.Set k)
mapReverse m = foldl (\m (k,v) -> Map.insertWith Set.union v (Set.singleton k) m) Map.empty (Map.toList m)

mapReverseAll :: (Ord v, Ord k) => Map.Map k (Set.Set v) -> Map.Map v (Set.Set k)
mapReverseAll m =
  let kvs = Map.map Set.toList m        -- Map.Map k [v]
      kvs' = Map.toList kvs             -- [(k, [v])]
      vks = [[(vv, Set.singleton kk)] | (kk, vvs) <- kvs', vv <- vvs]  -- [[(v, Set.Set k)]]
      vks' = concat vks                 -- [(v, Set.Set k)]
  in Map.fromListWith Set.union vks'

-- Sudoku-style constraint narrowing:
-- given a map of items to candidates, with a unique a->b solution available, work out that unique mapping
narrowCandidateMap :: (Eq a, Ord b) => Map.Map a (Set.Set b) -> Either (Map.Map a (Set.Set b)) (Map.Map a b)
narrowCandidateMap m =
  let singletons = Map.filter (\cs -> Set.size cs == 1) m
  in  if Map.size singletons == 0 then error "no unique solution"
      else if Map.size singletons == Map.size m then Right $ finalise m
      else
        let uniquelyAssigned = Map.toList singletons & map snd & Set.unions
            narrowed = Map.map (narrow uniquelyAssigned) m
        in if narrowed /= m then narrowCandidateMap narrowed else Left m
  where narrow singletons cs
          | Set.size cs == 1 = cs
          | otherwise        = Set.difference cs singletons
        finalise m = Map.map (head . Set.toList) m


infixl 8 <<<<
(<<<<) :: ReadP p1 -> ReadP p2 -> ReadP p1
p1 <<<< p2 = do
  x <- p1
  _ <- p2
  return x

infixl 8 >>>>
(>>>>) :: ReadP p1 -> ReadP p2 -> ReadP p2
p1 >>>> p2 = do
  _ <- p1
  p2

infixl 6 <<>>
(<<>>) :: ReadP p1 -> ReadP p2 -> ReadP (p1, p2)
p1 <<>> p2 = do
  x <- p1
  y <- p2
  return (x, y)

infixl 8 <<!!
(<<!!) :: ReadP p1 -> (p1 -> p2) -> ReadP p2
p1 <<!! f = do
  x <- p1
  return $ f x

loadMap :: (Num a, Ord a, Enum a) => [String] -> Map.Map (a, a) Char
loadMap ls = [((x, y), c) | (y, line) <- [0..] `zip` ls, (x, c) <- [0..] `zip` line] & Map.fromList

loadMapWith :: (Num a, Ord a, Enum a) => ((a, a) -> Char -> b) -> [String] -> Map.Map (a, a) b
loadMapWith f ls = loadMap ls & Map.mapWithKey f

boundMap :: (Num a, Ord a, Enum a) => Map.Map (a, a) c -> ((a, a), (a, a))
boundMap m =
  let xs = Map.keysSet m & Set.map fst
      ys = Map.keysSet m & Set.map snd
  in ((minimum xs, maximum xs), (minimum ys, maximum ys))

offsetMap :: (Num a, Ord a, Enum a) => (a, a) -> Map.Map (a, a) c -> Map.Map (a, a) c
offsetMap (dx, dy) = Map.mapKeys (\(x,y) -> (x+dx, y+dy))

-- make the map top-left be (0, 0)
normaliseMap :: (Num a, Ord a, Enum a) => Map.Map (a, a) c -> Map.Map (a, a) c
normaliseMap m =
  let ((x0, _), (y0, _)) = boundMap m
  in  offsetMap (-x0, -y0) m

rotateLeftMap :: (Num a, Ord a, Enum a) => Map.Map (a, a) c -> Map.Map (a, a) c
rotateLeftMap m = m & Map.mapKeys (\(x, y) -> (y, -x)) & normaliseMap

rotateRightMap :: (Num a, Ord a, Enum a) => Map.Map (a, a) c -> Map.Map (a, a) c
rotateRightMap m = m & Map.mapKeys (\(x, y) -> (-y, x)) & normaliseMap

rotate180Map :: (Num a, Ord a, Enum a) => Map.Map (a, a) c -> Map.Map (a, a) c
rotate180Map m = m & Map.mapKeys (\(x, y) -> (-x, -y)) & normaliseMap

flipXMap :: (Num a, Ord a, Enum a) => Map.Map (a, a) c -> Map.Map (a, a) c
flipXMap m = m & Map.mapKeys (\(x, y) -> (-x, y)) & normaliseMap

flipYMap :: (Num a, Ord a, Enum a) => Map.Map (a, a) c -> Map.Map (a, a) c
flipYMap m = m & Map.mapKeys (\(x, y) -> (x, -y)) & normaliseMap

-- return a normalised subgrid
subMap ((x0, x1), (y0, y1)) grid =
  Map.fromList [((x, y), grid Map.! (x, y)) | x <- [x0..x1], y <- [y0..y1]] & normaliseMap

-- return a grid of grids
diceMap (dx, dy) grid =
  let ((x0, x1), (y0, y1)) = boundMap grid
      (sx, sy) = (x1 - x0 + 1, y1 - y0 + 1)
      (nx, ny) = (sx `div` dx, sy `div` dy)
  in
  -- trace ("(x0,x1)=" ++ show (x0,x1) ++ " dx=" ++ show dx ++ " sx=" ++ show sx ++ " nx=" ++ show nx) $
  Map.fromList [
    ((i, j), subMap ((i * dx, i * dx + dx - 1), (j * dy, j * dy + dy - 1)) grid) |
      i <- [0 .. nx-1], j <- [0..ny-1]
  ]

-- return a single grid
undiceMap grid =
  let ((x0, x1), (y0, y1)) = boundMap (grid Map.! (0,0))
      (dx, dy) = (x1 - x0 + 1, y1 - y0 + 1)
  in  Map.toList grid
    & map (\((i, j), g) -> offsetMap (i * dx, j * dy) g)
    & Map.unions


wordParser :: ReadP String
wordParser = many1 (satisfy (/= ' '))


-- extended euclidean algorithm: given a, b, return (g, s, t) such that g = gcd(a, b) and as + bt = g
euc :: Integer -> Integer -> (Integer, Integer, Integer)
euc a b = xeuc (1, a, 1, 0) (1, b, 0, 1)
  where
    xeuc (q0, r0, s0, t0) (q1, r1, s1, t1)
      | r1 == 0 = (r0, s0, t0)
      | otherwise = let (q, r) = divMod r0 r1
                    in  xeuc (q1, r1, s1, t1) (q, r, s0 - q * s1, t0 - q * t1)

-- Solve ax + by = c, giving ((s, t), (s', t')) where x = sr + t, y=s'r + t' is a solution for any r
solveDiophantine :: Integer -> Integer -> Integer -> ((Integer, Integer), (Integer, Integer))
solveDiophantine a b c =
  let (g, s, t) = euc a b                  -- find s and t such that as + bt = gcd(a,b)
      (x1, y1)  = (s * c `div` g, t * c `div` g)   -- find an example x1, y1 such that a.x1 + b.y1 = c
  in ((-b `div` g, x1), (a `div` g, y1))


{- If we have regular solutions for values of t:
    t = mx + f
    t = ny + g
   Then
    mx + f = ny + g, or
    mx + (-n)y = g - f
   then find a, b such that
    x = ar + b     (along with y = cr + d)
   satisfies both of these.
   Substitute in so that
    t = mx + f = mar + (mb + f)
   The result, (m', f') = (ma, mb + f)
-}

combineDiophantine (m, f) (n, g) =
  let ((a, b), _) = solveDiophantine m (-n) (g - f)
      m' = m * a
      f' = m * b + f
  in  (abs m', f' `mod` (abs m'))


-- Re-usable BFS
search :: (Ord c, Ord s, Show c, Show s, Ord t) =>
       ((c, s) -> Set.Set (c, s))               -- a function that returns a set of potential next states
    -> ((c, s) -> Bool)                         -- a function that tells if a state is one searched for
    -> (s -> t)                                 -- a state-summarising function to prevent repeating searches of equivalent states
    -> (c, s)                                   -- starting cost and state
    -> Either (Set.Set t) (c, s, Set.Set t)     -- a solution, if one is found
search nextStates satisfying summariseState (startCost, startState) =
  bfs0 nextStates satisfying summariseState Set.empty (H.singleton (startCost, startState))
  where
  bfs0 :: (Ord c, Ord s, Show c, Show s, Ord t) =>
         ((c, s) -> Set.Set (c, s))               -- a function that returns a set of potential next states
      -> ((c, s) -> Bool)                         -- a function that tells if a state is one searched for
      -> (s -> t)                                 -- a state-summarising function to prevent repeating searches of equivalent states
      -> Set.Set t                                -- equivalent states already processed
      -> H.MinPrioHeap c s                        -- search queue
      -> Either (Set.Set t) (c, s, Set.Set t)     -- a solution, if one is found
  bfs0 nextStates satisfying summariseState seenStates stateQueue
    | H.null stateQueue = Left seenStates
    | otherwise =
      let Just ((cost, state), q') = H.view stateQueue
          summary = summariseState state
          nexts = nextStates (cost, state)
          q'' = Set.foldr H.insert q' nexts
          seen' = Set.insert summary seenStates
      in
      if satisfying (cost, state)
      then Right (cost, state, seenStates)
      else if Set.member summary seenStates
      then -- trace ("skipping seen state at " ++ (show cost)) $
           bfs0 nextStates satisfying summariseState seenStates q'
      else -- trace ("bfs searches: " ++ (show cost)) $
           bfs0 nextStates satisfying summariseState seen' q''

bfs nextStates satisfying summariseState startState =
  case search nextStates satisfying summariseState startState of
    Left _ -> Nothing
    Right (cost, state, _) -> Just (cost, state)

-- return all the states visited until we reached the satisfying one
--flood :: ((c, s) -> Set.Set (c, s))               -- a function that returns a set of potential next states
--      -> ((c, s) -> Bool)                         -- a function that tells if a state is one searched for
--      -> (s -> t)                                 -- a state-summarising function to prevent repeating searches of equivalent states
--      -> (c, s)                                   -- starting cost and state
--      -> Either (Set.Set t) (Set.Set t)
flood nextStates satisfying summariseState startState =
  case search nextStates satisfying summariseState startState of
    Left r -> Left r
    Right (_, _, reached) -> Right reached

-- return all reachable states
floodFill nextStates startState =
  case search nextStates (const False) id startState of
    Left r -> r
    Right (_, _, reached) -> reached


-- a* search. Provide a cost function and a heuristic. If the heuristic function is admissible,
-- meaning that it never overestimates the actual cost to get to the goal, A* is guaranteed to
-- return a least-cost path from start to goal.
aStar :: (Ord s, Ord c, Num c, Ord t)
      => (s -> Set.Set s)
      -> (s -> Bool)
      -> (s -> c)
      -> (s -> c)
      -> (s -> t)
      -> s
      -> Maybe s
aStar nextStates finished cost heuristic summariseState startState =
  aStar' nextStates finished cost heuristic summariseState
         Set.empty (H.singleton (cost startState + heuristic startState, startState))

aStar' :: (Ord s, Ord c, Num c, Ord t)
      => (s -> Set.Set s)
      -> (s -> Bool)
      -> (s -> c)
      -> (s -> c)
      -> (s -> t)
      -> Set.Set t
      -> H.MinPrioHeap c s
      -> Maybe s
aStar' nextStates finished cost heuristic summariseState visited queue
  | H.null queue = Nothing
  | otherwise =
    let Just ((f, state), queue') = H.view queue
        summary = summariseState state
        queue'' = foldl (\q s' -> H.insert (cost s' + heuristic s', s') q) queue' (nextStates state)
    in  if finished state then Just state
        else if Set.member summary visited
        then aStar' nextStates finished cost heuristic summariseState visited queue'
        else aStar' nextStates finished cost heuristic summariseState (Set.insert summary visited) queue''


-- handy lists of directions
orthogonalMoves = [(-1, 0), (1, 0), (0, -1), (0, 1)]
kingsMoves = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
neighbours dirs (x, y) = [(x+dx, y + dy) | (dx,dy) <- dirs]
neighboursIn dirs grid (x,y) = neighbours dirs (x,y) & filter (`Map.member` grid)
swap (a,b) = (b,a)



squareRoot :: Integer -> Integer
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
   let twopows = iterate (^2) 2
       (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
       isRoot r  =  r^2 <= n && n < (r+1)^2
  in  head $ dropWhile (not . isRoot) iters


-- hex to binary
hexToBin :: String -> String
hexToBin hs = concatMap fromHex hs

fromHex '0' = "0000"
fromHex '1' = "0001"
fromHex '2' = "0010"
fromHex '3' = "0011"
fromHex '4' = "0100"
fromHex '5' = "0101"
fromHex '6' = "0110"
fromHex '7' = "0111"
fromHex '8' = "1000"
fromHex '9' = "1001"
fromHex 'A' = "1010"
fromHex 'B' = "1011"
fromHex 'C' = "1100"
fromHex 'D' = "1101"
fromHex 'E' = "1110"
fromHex 'F' = "1111"
fromHex 'a' = "1010"
fromHex 'b' = "1011"
fromHex 'c' = "1100"
fromHex 'd' = "1101"
fromHex 'e' = "1110"
fromHex 'f' = "1111"
