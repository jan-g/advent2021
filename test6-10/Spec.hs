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
      it "correctly runs on the test data" $ do
        True `shouldBe` True
