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
        hexToBin "D2FE28" `shouldBe` "110100101111111000101000"
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

    describe "day 19" $ do
      let
        ex = "--- scanner 0 ---\n\
             \404,-588,-901\n\
             \528,-643,409\n\
             \-838,591,734\n\
             \390,-675,-793\n\
             \-537,-823,-458\n\
             \-485,-357,347\n\
             \-345,-311,381\n\
             \-661,-816,-575\n\
             \-876,649,763\n\
             \-618,-824,-621\n\
             \553,345,-567\n\
             \474,580,667\n\
             \-447,-329,318\n\
             \-584,868,-557\n\
             \544,-627,-890\n\
             \564,392,-477\n\
             \455,729,728\n\
             \-892,524,684\n\
             \-689,845,-530\n\
             \423,-701,434\n\
             \7,-33,-71\n\
             \630,319,-379\n\
             \443,580,662\n\
             \-789,900,-551\n\
             \459,-707,401\n\
             \\n\
             \--- scanner 1 ---\n\
             \686,422,578\n\
             \605,423,415\n\
             \515,917,-361\n\
             \-336,658,858\n\
             \95,138,22\n\
             \-476,619,847\n\
             \-340,-569,-846\n\
             \567,-361,727\n\
             \-460,603,-452\n\
             \669,-402,600\n\
             \729,430,532\n\
             \-500,-761,534\n\
             \-322,571,750\n\
             \-466,-666,-811\n\
             \-429,-592,574\n\
             \-355,545,-477\n\
             \703,-491,-529\n\
             \-328,-685,520\n\
             \413,935,-424\n\
             \-391,539,-444\n\
             \586,-435,557\n\
             \-364,-763,-893\n\
             \807,-499,-711\n\
             \755,-354,-619\n\
             \553,889,-390\n\
             \\n\
             \--- scanner 2 ---\n\
             \649,640,665\n\
             \682,-795,504\n\
             \-784,533,-524\n\
             \-644,584,-595\n\
             \-588,-843,648\n\
             \-30,6,44\n\
             \-674,560,763\n\
             \500,723,-460\n\
             \609,671,-379\n\
             \-555,-800,653\n\
             \-675,-892,-343\n\
             \697,-426,-610\n\
             \578,704,681\n\
             \493,664,-388\n\
             \-671,-858,530\n\
             \-667,343,800\n\
             \571,-461,-707\n\
             \-138,-166,112\n\
             \-889,563,-600\n\
             \646,-828,498\n\
             \640,759,510\n\
             \-630,509,768\n\
             \-681,-892,-333\n\
             \673,-379,-804\n\
             \-742,-814,-386\n\
             \577,-820,562\n\
             \\n\
             \--- scanner 3 ---\n\
             \-589,542,597\n\
             \605,-692,669\n\
             \-500,565,-823\n\
             \-660,373,557\n\
             \-458,-679,-417\n\
             \-488,449,543\n\
             \-626,468,-788\n\
             \338,-750,-386\n\
             \528,-832,-391\n\
             \562,-778,733\n\
             \-938,-730,414\n\
             \543,643,-506\n\
             \-524,371,-870\n\
             \407,773,750\n\
             \-104,29,83\n\
             \378,-903,-323\n\
             \-778,-728,485\n\
             \426,699,580\n\
             \-438,-605,-362\n\
             \-469,-447,-387\n\
             \509,732,623\n\
             \647,635,-688\n\
             \-868,-804,481\n\
             \614,-800,639\n\
             \595,780,-596\n\
             \\n\
             \--- scanner 4 ---\n\
             \727,592,562\n\
             \-293,-554,779\n\
             \441,611,-461\n\
             \-714,465,-776\n\
             \-743,427,-804\n\
             \-660,-479,-426\n\
             \832,-632,460\n\
             \927,-485,-438\n\
             \408,393,-506\n\
             \466,436,-512\n\
             \110,16,151\n\
             \-258,-428,682\n\
             \-393,719,612\n\
             \-211,-452,876\n\
             \808,-476,-593\n\
             \-575,615,604\n\
             \-485,667,467\n\
             \-680,325,-822\n\
             \-627,-443,-432\n\
             \872,-547,-609\n\
             \833,512,582\n\
             \807,604,487\n\
             \839,-516,451\n\
             \891,-625,532\n\
             \-652,-548,-490\n\
             \30,-46,-14" & lines
        ss = Day19.parse ex
        s0 = ss Map.! 0
        s1 = ss Map.! 1
        s2 = ss Map.! 2
        s3 = ss Map.! 3
        s4 = ss Map.! 4
      it "aligns scanner 0 and 1" $ do
        let
          Just (s,o,ps) = Day19.fitScanner [(0, (0,0,0), s0)] [(1, s1)]
        o `shouldBe` (68,-1246,-43)
      it "aligns scanner 1 and 4" $ do
        let
          Just (s,o,ps) = Day19.fitScanner [(0, (0,0,0), s0)] [(1, s1)]
          Just (s',o',ps') = Day19.fitScanner [(0, (0,0,0), s0), (s,o,ps)] [(4,s4)]          
        o' `shouldBe` (-20,-1133,1061) 
      it "aligns everything" $ do
        let
          r = Day19.fitAll [(0, (0,0,0), s0)] (Map.delete 0 ss)
          r' = r & map (\(s,o,_) -> (s,o)) & Map.fromList
        r' `shouldBe` Map.fromList [ (0, (0,0,0))
                                  , (1, (68,-1246,-43))
                                  , (2, (1105,-1205,1229))
                                  , (3, (-92,-2380,-20))
                                  , (4, (-20,-1133,1061))
                                  ]                                  
        Set.size (Day19.allPoints r) `shouldBe` 79
      
      it "does part b" $ do
        Day19.day19b ex `shouldBe` 3621