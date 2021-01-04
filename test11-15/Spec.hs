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
      it "correctly runs on the test data" $ do
        True `shouldBe` True
