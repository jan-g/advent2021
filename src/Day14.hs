module Day14 where

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
import Data.Counter as C

import Debug.Trace (trace)

import Lib


{-
--- Day 14: Extended Polymerization ---

The incredible pressures at this depth are starting to put a strain on your submarine. The submarine has polymerization equipment that would produce suitable materials to reinforce the submarine, and the nearby volcanically-active caves should even have the necessary input elements in sufficient quantities.

The submarine manual contains instructions for finding the optimal polymer formula; specifically, it offers a polymer template and a list of pair insertion rules (your puzzle input). You just need to work out what polymer would result after repeating the pair insertion process a few times.

For example:

NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C

The first line is the polymer template - this is the starting point of the process.

The following section defines the pair insertion rules. A rule like AB -> C means that when elements A and B are immediately adjacent, element C should be inserted between them. These insertions all happen simultaneously.

So, starting with the polymer template NNCB, the first step simultaneously considers all three pairs:

    The first pair (NN) matches the rule NN -> C, so element C is inserted between the first N and the second N.
    The second pair (NC) matches the rule NC -> B, so element B is inserted between the N and the C.
    The third pair (CB) matches the rule CB -> H, so element H is inserted between the C and the B.

Note that these pairs overlap: the second element of one pair is the first element of the next pair. Also, because all pairs are considered simultaneously, inserted elements are not considered to be part of a pair until the next step.

After the first step of this process, the polymer becomes NCNBCHB.

Here are the results of a few steps using the above rules:

Template:     NNCB
After step 1: NCNBCHB
After step 2: NBCCNBBBCBHCB
After step 3: NBBBCNCCNBBNBNBBCHBHHBCHB
After step 4: NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB

This polymer grows quickly. After step 5, it has length 97; After step 10, it has length 3073. After step 10, B occurs 1749 times, C occurs 298 times, H occurs 161 times, and N occurs 865 times; taking the quantity of the most common element (B, 1749) and subtracting the quantity of the least common element (H, 161) produces 1749 - 161 = 1588.

Apply 10 steps of pair insertion to the polymer template and find the most and least common elements in the result. What do you get if you take the quantity of the most common element and subtract the quantity of the least common element?

To begin, get your puzzle input.
-}

parse ls =
  let
    [[start], rules] = splitOn [""] ls
    changes = mapMaybe (quickParse parseLine) rules
  in
    (start, Map.fromList changes)

parseLine :: ReadP ((Char, Char), Char)
parseLine = do
  c1 <- satisfy isUpper
  c2 <- satisfy isUpper
  string " -> "
  c3 <- satisfy isUpper
  eof
  return ((c1, c2), c3)

step _ [] = []
step _ [a] = [a]
step rules (a:b:cs) = a:(rules Map.! (a, b)):step rules (b:cs)

day14 ls =
  let (start, rules) = parse ls
      results = iterate (step rules) start
      tenth = results !! 10
      cs = C.count tenth
      cs' = mapReverse cs
      (lowest, _) = Map.findMin cs'
      (highest, _) = Map.findMax cs'
  in
    highest - lowest


{-
--- Part Two ---

The resulting polymer isn't nearly strong enough to reinforce the submarine. You'll need to run more steps of the pair insertion process; a total of 40 steps should do it.

In the above example, the most common element is B (occurring 2192039569602 times) and the least common element is H (occurring 3849876073 times); subtracting these produces 2188189693529.

Apply 40 steps of pair insertion to the polymer template and find the most and least common elements in the result. What do you get if you take the quantity of the most common element and subtract the quantity of the least common element?
-}

{-
The idea here is that at any point we have a string which can be viewed as a bunch of pairs. Bracketing the string by
terminal '$' characters: $ABCDABCDABCD$, we can view this as pairs $A AB BC CD DA ... D$

What matters isn't the order of those pairs, but rather, their total counts: eg, the counts above include
$A: 1, D$: 1, AB: 3, DA: 2

Given a set of expansion rules, we might know that AB produces ACB. In other words, if we began with a string comprising
`n` copies of AB, that would contribute `n` copies of each of AC anc CB to the next stage.

Including the terminal pairs, then, if we iterate this process of taking each pair and having it contribute to the count
of other pairs in the next generation, we end up counting each letter in the resulting string twice. So halve those
counts and the results give you a fast way to find the values we're after.

The rest is just a question of constructing and combining maps to represent the counts.
-}

makeExpansionRules :: Map.Map (Char, Char) Char -> Map.Map (Char, Char) (Map.Map (Char, Char) Integer)
makeExpansionRules rules =
  let
    -- find all letters
    letters = Map.toList rules & map snd & Set.fromList & Set.toList
    -- terminal pairs remain unchanged.
    starts = Map.fromList [(('$', c), Map.singleton ('$', c) 1) | c <- letters]
    ends = Map.fromList [((c, '$'), Map.singleton (c, '$') 1) | c <- letters]
    -- each middle pair produces either two new pairs (one each) or two of a doubled output.
    middles = Map.fromList $ do
      c1 <- letters
      c2 <- letters
      let middle = rules Map.! (c1, c2)
      pure $ ((c1, c2), C.count [(c1, middle), (middle, c2)])
    expansions = Map.unions [starts, ends, middles]
  in
    expansions

makePairCounts :: String -> Map.Map (Char, Char) Integer
makePairCounts start =
  -- turn ABCABC into $A AB BC CA AB BC C$ and count each unique pair.
  let
    s = "$" ++ start ++ "$"
    pairs = s `zip` (tail s)
  in
    C.count pairs

bumpCounts :: Map.Map (Char, Char) (Map.Map (Char, Char) Integer) -> Map.Map (Char, Char) Integer -> Map.Map (Char, Char) Integer
bumpCounts rules cs =
  -- tot up the contributions from each pair
  Map.unionsWith (+) newEntries
  where
    newEntries :: [Map.Map (Char, Char) Integer]
    newEntries = do
      -- for each pair, we have `c` copies of it on the way in
      ((c1, c2), c) <- cs & Map.toList
      let expansions = rules Map.! (c1, c2)
      -- that'll produce either `c` copies of each output pair, or `2c` copies for a double-letter
      (item, c') <- expansions & Map.toList
      return $ Map.singleton item (c * c')

countLetters :: Map.Map (Char, Char) Integer -> Map.Map Char Integer
countLetters cs =
  let
    -- An alternative here is just to take the first, or the second, occurrence.
    firsts = cs & Map.toList & map (\((c, _), n) -> (c, n))
    lasts = cs & Map.toList & map (\((_, c), n) -> (c, n))
    cs' = firsts ++ lasts
    ms = map (\(c, n) -> Map.singleton c n) cs'
  in
    -- normalise the count of occurrences, taking into consideration that everything is double-counted
    Map.unionsWith (+) ms & Map.map (`div` 2)

summariseItem :: Map.Map (Char, Char) Integer -> Integer
summariseItem item =
  let
    -- work out the most and least common letters and return the difference in their counts
    cs = item & countLetters & Map.delete '$'
    revs = mapReverse cs
    (l, _) = Map.findMin revs
    (h, _) = Map.findMax revs
  in
    h - l

day14b ls =
  let
    (start, rules) = parse ls
    expansions = makeExpansionRules rules
    s0 = makePairCounts start
    iters = iterate (bumpCounts expansions) s0
  in
    summariseItem $ iters !! 40