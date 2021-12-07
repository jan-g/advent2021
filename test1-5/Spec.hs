import Test.Hspec
import Control.Exception (evaluate)

import Data.Array
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad
import Data.List.Split (splitOn)
import Data.List as L
import Data.Maybe (catMaybes, fromJust)
import qualified Data.ByteString.UTF8 as BSU
import qualified Text.ParserCombinators.ReadP as P
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

import Lib
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5


main :: IO ()
main =
  hspec $ do
    describe "Day 1" $ do
      let input = "199\n200\n208\n210\n200\n207\n240\n269\n260\n263\n"
      it "correctly runs on the test data" $ do
        (Day1.day1 $ lines input) `shouldBe` 7
      it "correctly computes the windows on the test data" $ do
        let ns = Day1.parse $ lines input
        Day1.slide ns `shouldBe` [607, 618, 618, 617, 647, 716, 769, 792]

    describe "Day 2" $ do
      let input = "forward 5\n\
                  \down 5\n\
                  \forward 8\n\
                  \up 3\n\
                  \down 8\n\
                  \forward 2\n\
                  \"
      let ds = Day2.parse $ lines input
      it "parses the deltas" $ do
        ds `shouldBe` [(5,0), (0,5), (8,0), (0,-3), (0,8), (2,0)]
      it "sums the deltas" $ do
        Day2.tot ds `shouldBe` (15,10)
      it "solves part a" $ do
        Day2.day2 (lines input) `shouldBe` 150
      it "runs the example on day2b" $ do
        Day2.tot2 ds `shouldBe` (15,60,10)

    describe "day 3" $ do
      let example = "00100\n\
                    \11110\n\
                    \10110\n\
                    \10111\n\
                    \10101\n\
                    \01111\n\
                    \00111\n\
                    \11100\n\
                    \10000\n\
                    \11001\n\
                    \00010\n\
                    \01010\n\
                    \" & lines
      it "solves part a" $ do
        Day3.gamma example `shouldBe` 22
        Day3.epsilon example `shouldBe` 9
      it "solves part b" $ do
        Day3.oxygen example `shouldBe` 23
        Day3.co2 example `shouldBe` 10

    describe "day 4" $ do
      let example = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\
                    \\n\
                    \22 13 17 11  0\n\
                    \ 8  2 23  4 24\n\
                    \21  9 14 16  7\n\
                    \ 6 10  3 18  5\n\
                    \ 1 12 20 15 19\n\
                    \\n\
                    \ 3 15  0  2 22\n\
                    \ 9 18 13 17  5\n\
                    \19  8  7 25 23\n\
                    \20 11 10 24  4\n\
                    \14 21 16 12  6\n\
                    \\n\
                    \14 21 17 24  4\n\
                    \10 16 15  9 19\n\
                    \18  8 23 26 20\n\
                    \22 11 13  6  5\n\
                    \ 2  0 12  3  7\n\
                    \" & lines
          (ns, boards) = Day4.parse example
      it "parses" $ do
        length boards `shouldBe` 3
      it "plays" $ do
        let (acc, b) = Day4.play ns (Set.fromList boards) []
        acc `shouldBe` [24, 21, 14, 0, 2, 23, 17, 11, 5, 9, 4, 7]
        Day4.score acc b `shouldBe` 188
      it "winnows" $ do
        let (a:as, b:_) = Day4.winnow ns (Set.fromList boards) [] []
        a `shouldBe` 13
        Set.size b `shouldBe` 1
        let b' = Set.toList b & head
        Day4.score (a:as) b' `shouldBe` 148

    describe "day 5" $ do
      let example = "0,9 -> 5,9\n\
                    \8,0 -> 0,8\n\
                    \9,4 -> 3,4\n\
                    \2,2 -> 2,1\n\
                    \7,0 -> 7,4\n\
                    \6,4 -> 2,0\n\
                    \0,9 -> 2,9\n\
                    \3,4 -> 1,4\n\
                    \0,0 -> 8,8\n\
                    \5,5 -> 8,2" & lines
      it "calculates overlaps" $ do
        Day5.day5 example `shouldBe` 5
      it "calculates overlaps with diagonals" $ do
        Day5.day5b example `shouldBe` 12
