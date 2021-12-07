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
