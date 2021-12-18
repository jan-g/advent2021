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

    describe "day 18" $ do
      let
        p = Day18.Pair
        s = Day18.Single
      it "shows" $ do
        show (p (s 1) (s 2)) `shouldBe` "[1,2]"
        show (s 1) `shouldBe` "1"
        show (p (p (p (p (p (s 9) (s 8)) (s 1)) (s 2)) (s 3)) (s 4)) `shouldBe` "[[[[[9,8],1],2],3],4]"

      let
        example = "[[[[[9,8],1],2],3],4]\n\
                  \[7,[6,[5,[4,[3,2]]]]]\n\
                  \[[6,[5,[4,[3,2]]]],1]\n\
                  \[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]\n\
                  \[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]\n\
                  \" & lines
      forM_ example $ \line ->
        it ("parses " ++ line) $ do
          (Day18.parse [line] & head & show) `shouldBe` line

      let
        eg1 = [ ("[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]")
              , ("[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[7,0]]]]")
              , ("[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[7,0]]],3]")
              , ("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
              , ("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")
              ]
      forM_ eg1 $ \(before, after) -> do
        it ("explodes " ++ before) $ do
          let
            [p1] = Day18.parse [before]
            [p2] = Day18.parse [after]
          Day18.explode p1 `shouldBe` p2
        
      let
        eg2 = [ ("10", "[5,5]")
              , ("11", "[5,6]")
              , ("12", "[6,6]")
              , ("[12,11]", "[[6,6],11]")
              , ("[2,11]", "[2,[5,6]]")
              ]
      forM_ eg2 $ \(before, after) -> do
        let
          [p1] = Day18.parse [before]
          [p2] = Day18.parse [after]
        it ("splits " ++ before) $ do
          Day18.splitPair p1 `shouldBe` p2

      it "adds up" $ do
        let
          [p1] = Day18.parse ["[[[[4,3],4],4],[7,[[8,4],9]]]"]
          [p2] = Day18.parse ["[1,1]"]
          [ex] = Day18.parse ["[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"]
        Day18.add p1 p2 `shouldBe` ex

      it "does example 1" $ do
        let
          ex = "[1,1]\n\
               \[2,2]\n\
               \[3,3]\n\
               \[4,4]" & lines
          ps = Day18.parse ex
        (Day18.tot ps & show) `shouldBe` "[[[[1,1],[2,2]],[3,3]],[4,4]]"

      it "does example 2" $ do
        let
          ex = "[1,1]\n\
               \[2,2]\n\
               \[3,3]\n\
               \[4,4]\n\
               \[5,5]" & lines
          ps = Day18.parse ex
        (Day18.tot ps & show) `shouldBe` "[[[[3,0],[5,3]],[4,4]],[5,5]]"

      it "does example 3" $ do
        let
          ex = "[1,1]\n\
               \[2,2]\n\
               \[3,3]\n\
               \[4,4]\n\
               \[5,5]\n\
               \[6,6]" & lines
          ps = Day18.parse ex
        (Day18.tot ps & show) `shouldBe` "[[[[5,0],[7,4]],[5,5]],[6,6]]"

      it "does the bigger example" $ do
        let
          ex = "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]\n\
               \[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]\n\
               \[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]\n\
               \[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]\n\
               \[7,[5,[[3,8],[1,4]]]]\n\
               \[[2,[2,2]],[8,[8,1]]]\n\
               \[2,9]\n\
               \[1,[[[9,3],9],[[9,0],[0,7]]]]\n\
               \[[[5,[7,4]],7],1]\n\
               \[[[[4,2],2],6],[8,7]]" & lines
          ps = Day18.parse ex
        (Day18.tot ps & show) `shouldBe` "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"
        
      forM_ [ ("[[1,2],[[3,4],5]]", 143)
            , ("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]", 1384)
            , ("[[[[1,1],[2,2]],[3,3]],[4,4]]", 445)
            , ("[[[[3,0],[5,3]],[4,4]],[5,5]]", 791)
            , ("[[[[5,0],[7,4]],[5,5]],[6,6]]", 1137)
            , ("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]", 3488)
            ] $ \(before, after) ->
        it ("computes the magnitude of " ++ before) $ do
          let
            [p] = Day18.parse [before]
          Day18.magnitude p `shouldBe` after  

      let
        ex = "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]\n\
             \[[[5,[2,8]],4],[5,[[9,9],0]]]\n\
             \[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]\n\
             \[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]\n\
             \[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]\n\
             \[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]\n\
             \[[[[5,4],[7,7]],8],[[8,3],8]]\n\
             \[[9,3],[[9,9],[6,[4,9]]]]\n\
             \[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]\n\
             \[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]" & lines
      it "computes the final example" $ do
        Day18.day18 ex `shouldBe` 4140
      
      it "finds the largest pairwise magnitude" $ do
        Day18.day18b ex `shouldBe` 3993

