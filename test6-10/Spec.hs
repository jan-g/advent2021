{-# Language LambdaCase #-}
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
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10


main :: IO ()
main =
  hspec $ do
    describe "Day 6" $ do
      let example = "3,4,3,1,2\n" & lines
          ages = Day6.parse example & Day6.ageCounts
      it "correctly updates for one day" $ do
        Day6.bumpCounts ages `shouldBe` Map.fromList [(2,2), (3,1), (0,1), (1,1)]
      it "correctly computes the nth day" $ do
        Day6.nDays ages 3 `shouldBe` Map.fromList[(0,2), (1,1), (5,1), (6,1), (7,1), (8,1)]
      it "counts fish" $ do
        Day6.countFish ages `shouldBe` 5
      it "correctly sums fish on day 18" $ do
        Day6.countFish (Day6.nDays ages 18) `shouldBe` 26
      it "works forward to day 80" $ do
        Day6.countFish (Day6.nDays ages 80) `shouldBe` 5934

    describe "day 7" $ do
      let example = "16,1,2,0,4,2,7,1,2,14" & lines
      it "works out minimum costs" $ do
        Day7.day7 example `shouldBe` (2, 37)
      it "works out minimum type b costs" $ do
        Day7.day7b example `shouldBe` (5, 168)

    describe "day 8" $ do
      let small = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf\n" & lines
          large = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\n\
                  \edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\n\
                  \fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\n\
                  \fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\n\
                  \aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\n\
                  \fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\n\
                  \dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\n\
                  \bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\n\
                  \egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\n\
                  \gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce\n\
                  \" & lines
          e1 = Day8.parse small
          e2 = Day8.parse large
          obvious = "abcefg cf acdeg acdfg bcdf abdfg abdefg acf abcdefg abcdfg | cf acdeg acdfg bcdf" & lines
          e3 = Day8.parse obvious
      it "counts the easy example" $ do
        Day8.day8 small `shouldBe` 0
        Day8.day8 large `shouldBe` 26
      it "identifies the digit sets" $ do
        let [(Day8.Line s _)] = e3
        (Day8.decode s) Map.! (Set.fromList "acdfg") `shouldBe` 3
      it "decodes digit sets into a single int" $ do
        let [(Day8.Line s d)] = e1
            decoder = Day8.decode s
        Day8.decodeDigits decoder d `shouldBe` 5353
      it "solves the digit sets" $ do
        let [(Day8.Line s _)] = e3
        (Day8.solve s) Map.! (Set.fromList "acdfg") `shouldBe` 3
      it "uses the permutaiton approach" $ do
        let [(Day8.Line s d)] = e1
            decoder = Day8.solve s
        Day8.decodeDigits decoder d `shouldBe` 5353
    
    describe "day 9" $ do
      let example = "2199943210\n\
                    \3987894921\n\
                    \9856789892\n\
                    \8767896789\n\
                    \9899965678\n\
                    \" & lines
      it "parses" $ do
        (Day9.parse example & drawMapWith (\_ (Just d) -> show d & head)) `shouldBe` example
      it "tots up risk" $ do
        Day9.day9 example `shouldBe` 15
      it "loctes regions" $ do
        let m = Day9.parse example
            rs = Day9.allRegions m
        (Set.toList rs & map Set.size & L.sort & reverse & take 3) `shouldBe` [14,9,9]

    describe "day 10" $ do
      it "parses complete chunks" $ do
        Day10.parseLine "()" `shouldBe` Day10.Complete ["()"]
        Day10.parseLine "[]" `shouldBe` Day10.Complete ["[]"]
        Day10.parseLine "([])" `shouldBe` Day10.Complete ["([])"]
        Day10.parseLine "{()()()}" `shouldBe` Day10.Complete ["{()()()}"]
        Day10.parseLine "<([{}])>" `shouldBe` Day10.Complete ["<([{}])>"]
        Day10.parseLine "[<>({}){}[([])<>]]" `shouldBe` Day10.Complete ["[<>({}){}[([])<>]]"]
        Day10.parseLine "(((((((((())))))))))" `shouldBe` Day10.Complete ["(((((((((())))))))))"]
      it "parses corrupt chunks" $ do
        Day10.parseLine "(]" `shouldBe` Day10.Corrupt [] "(" ']' ""
        Day10.parseLine "{()()()>" `shouldBe` Day10.Corrupt [] "{()()()" '>' ""
        Day10.parseLine "(((()))}" `shouldBe` Day10.Corrupt [] "(((()))" '}' ""
        Day10.parseLine "<([]){()}[{}])xxx" `shouldBe` Day10.Corrupt [] "<([]){()}[{}]" ')' "xxx"
      
      let example = "[({(<(())[]>[[{[]{<()<>>\n\
                    \[(()[<>])]({[<{<<[]>>(\n\
                    \{([(<{}[<>[]}>{[]{[(<()>\n\
                    \(((({<>}<{<{<>}{[]{[]{}\n\
                    \[[<[([]))<([[{}[[()]]]\n\
                    \[{[{({}]{}}([{[{{{}}([]\n\
                    \{<[[]]>}<{[{[{[]{()[[[]\n\
                    \[<(<(<(<{}))><([]([]()\n\
                    \<{([([[(<>()){}]>(<<{{\n\
                    \<{([{{}}[<[[[<>{}]]]>[]]\n\
                    \" & lines
      it "parses examples" $ do
        let ls = Day10.parse example
            incs = filter Day10.incomplete ls
            corr = filter Day10.corrupt ls
            comp = filter Day10.complete ls
        comp `shouldBe` []
        map (\case (Day10.Corrupt _ _ c _) -> c; x -> error $ show x) corr `shouldBe` "})])>"
      it "tots up scores for corrupt lines" $ do
        Day10.day10 example `shouldBe` 26397
      it "produces scores for incomplete lines" $ do
        let inc = Day10.parse example & filter Day10.incomplete
        (map Day10.incScore inc) `shouldBe` [288957, 5566, 1480781, 995444, 294]
      it "does part b" $ do
        Day10.day10b example `shouldBe` 288957