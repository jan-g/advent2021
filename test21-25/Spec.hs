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
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24
import qualified Day25


main :: IO ()
main =
  hspec $ do
    describe "Day 21" $ do
      it "correctly runs on the test data" $ do
        Day21.move 0 Day21.die [4, 8] [0,0] `shouldBe` (993, 745)

      it "correctly works out the scores for the two players" $ do
        let (w1, w2) = Day21.move' (Map.singleton ((0,4), (0,8)) 1) (0,0)
        min w1 w2 `shouldBe` 341960390180808
        max w1 w2 `shouldBe` 444356092776315
      
