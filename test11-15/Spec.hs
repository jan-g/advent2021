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
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15


main :: IO ()
main =
  hspec $ do
    describe "Day 11" $ do
      let unmap = drawMapWith (\_ (Just c) -> show c & head)
      let d0 = "11111\n\
               \19991\n\
               \19191\n\
               \19991\n\
               \11111" & lines
          d1 = "34543\n\
               \40004\n\
               \50005\n\
               \40004\n\
               \34543" & lines
          d2 = "45654\n\
               \51115\n\
               \61116\n\
               \51115\n\
               \45654" & lines       
          g0 = Day11.parse d0
          gs = iterate Day11.update g0
      it "runs the demos - day 0" $ do
        unmap (gs !! 0 & Day11.layout) `shouldBe` d0
      it "runs the demos - day 1" $ do
        unmap (gs !! 1 & Day11.layout) `shouldBe` d1
      it "runs the demos - day 2" $ do
        unmap (gs !! 2 & Day11.layout) `shouldBe` d2
        
      let example = "5483143223\n\
                    \2745854711\n\
                    \5264556173\n\
                    \6141336146\n\
                    \6357385478\n\
                    \4167524645\n\
                    \2176841721\n\
                    \6882881134\n\
                    \4846848554\n\
                    \5283751526\n\
                    \" & lines
          d1 = "6594254334\n\
               \3856965822\n\
               \6375667284\n\
               \7252447257\n\
               \7468496589\n\
               \5278635756\n\
               \3287952832\n\
               \7993992245\n\
               \5957959665\n\
               \6394862637\n\
               \" & lines
          d2 = "8807476555\n\
               \5089087054\n\
               \8597889608\n\
               \8485769600\n\
               \8700908800\n\
               \6600088989\n\
               \6800005943\n\
               \0000007456\n\
               \9000000876\n\
               \8700006848" & lines
          g = Day11.parse example
          gs = iterate Day11.update g
      it "correctly runs on the test data" $ do
        (Day11.layout (gs !! 1) & unmap) `shouldBe` d1
        Day11.count (gs !! 1) `shouldBe` 0
        (Day11.layout (gs !! 2) & unmap) `shouldBe` d2
        Day11.count (gs !! 2) `shouldBe` 35
      it "runs for 10 days" $ do
        Day11.count (gs !! 10) `shouldBe` 204        
      it "runs for 100 days" $ do
        Day11.count (gs !! 100) `shouldBe` 1656        
      it "finds the first day when all things flash" $ do
        Day11.day11b example `shouldBe` 195

    describe "Day 12" $ do
      let
        example = "start-A\n\
                 \start-b\n\
                 \A-c\n\
                 \A-b\n\
                 \b-d\n\
                 \A-end\n\
                 \b-end" & lines
      it "works out the small example" $ do
        let
          e1 = Day12.parse example
          ap = Day12.allPaths e1
        print e1
--        forM_ ap $ \p -> print $ "path is " ++ Day12.showPath p
        Set.size ap `shouldBe` 10
      
      let
        example2 = "dc-end\n\
                  \HN-start\n\
                  \start-kj\n\
                  \dc-start\n\
                  \dc-HN\n\
                  \LN-dc\n\
                  \HN-end\n\
                  \kj-sa\n\
                  \kj-HN\n\
                  \kj-dc" & lines
      it "works out the longer example" $ do
        let
          e2 = Day12.parse example2
          ap = Day12.allPaths e2
        Set.size ap `shouldBe` 19
      
      let
        example3 = "fs-end\n\
                  \he-DX\n\
                  \fs-he\n\
                  \start-DX\n\
                  \pj-DX\n\
                  \end-zg\n\
                  \zg-sl\n\
                  \zg-pj\n\
                  \pj-he\n\
                  \RW-he\n\
                  \fs-DX\n\
                  \pj-RW\n\
                  \zg-RW\n\
                  \start-pj\n\
                  \he-WI\n\
                  \zg-he\n\
                  \pj-fs\n\
                  \start-RW" & lines
      it "does the largest example" $ do
        let
          e3 = Day12.parse example3
          ap = Day12.allPaths e3
        Set.size ap `shouldBe` 226
      
      it "managed part 2 for the small example" $ do
        let
          e1 = Day12.parse example
          ap = Day12.allPaths' e1

--        forM_ ap $ \p -> print $ "path is " ++ Day12.showPath p
        Set.size ap `shouldBe` 36
      it "managed part 2 for the larger example" $ do
        let
          e2 = Day12.parse example2
          ap = Day12.allPaths' e2

        Set.size ap `shouldBe` 103
      it "managed part 2 for the largest example" $ do
        let
          e3 = Day12.parse example3
          ap = Day12.allPaths' e3

        Set.size ap `shouldBe` 3509
            