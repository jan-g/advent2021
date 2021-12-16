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
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20


main :: IO ()
main =
  hspec $ do
    describe "Day 16" $ do
      let
        op = Day16.op
        lit = Day16.lit

      it "correctly runs on the test data" $ do
        Day16.hexToBin "D2FE28" `shouldBe` "110100101111111000101000"
        Day16.parse ["D2FE28"] `shouldBe` lit 6 2021

      it "parses the other examples" $ do
        Day16.parse ["8A004A801A8002F478"] `shouldBe`
          op 4 2 [
            op 1 2 [
              op 5 2 [
                lit 6 15
              ]
            ]
          ]

      forM_ [ ("8A004A801A8002F478", 16)
            , ("620080001611562C8802118E34", 12)
            , ("C0015000016115A2E0802F182340", 23)
            , ("A0016C880162017C3686B18A3D4780", 31)
            ] $ \(p,v) ->
        it ("parses " ++ p) $ do
          Day16.versionSum (Day16.parse [p]) `shouldBe` v        

      forM_ [ ("C200B40A82", 3)
            , ("04005AC33890", 54)
            , ("880086C3E88112", 7)
            , ("CE00C43D881120", 9)
            , ("D8005AC2A8F0", 1)
            , ("F600BC2D8F", 0)
            , ("9C005AC2F8F0", 0)
            , ("9C0141080250320F1802104A08", 1)
            ] $ \(p,v) ->
        it ("evaluates " ++ p) $ do
          Day16.eval (Day16.parse [p]) `shouldBe` v
