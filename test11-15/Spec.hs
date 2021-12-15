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
import qualified Data.Counter as C

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

    describe "day 13" $ do
      let example = "6,10\n\
                    \0,14\n\
                    \9,10\n\
                    \0,3\n\
                    \10,4\n\
                    \4,11\n\
                    \6,0\n\
                    \6,12\n\
                    \4,1\n\
                    \0,13\n\
                    \10,12\n\
                    \3,4\n\
                    \3,0\n\
                    \8,4\n\
                    \1,10\n\
                    \2,14\n\
                    \8,10\n\
                    \9,0\n\
                    \\n\
                    \fold along y=7\n\
                    \fold along x=5" & lines
          (d, i) = Day13.parse example
      it "parses" $ do
        Set.size d `shouldBe` 18
      it "folds once" $ do
        Set.size (Day13.fold d $ head i) `shouldBe` 17

    describe "day 14" $ do
      let
        example = "NNCB\n\
                  \\n\
                  \CH -> B\n\
                  \HH -> N\n\
                  \CB -> H\n\
                  \NH -> C\n\
                  \HB -> C\n\
                  \HC -> B\n\
                  \HN -> C\n\
                  \NN -> C\n\
                  \BH -> H\n\
                  \NC -> B\n\
                  \NB -> B\n\
                  \BN -> B\n\
                  \BB -> N\n\
                  \BC -> B\n\
                  \CC -> N\n\
                  \CN -> C" & lines
      it "expands once" $ do
        let (start, rules) = Day14.parse example
        (iterate (Day14.step rules) start & take 5) `shouldBe` ["NNCB"
                                                               , "NCNBCHB"
                                                               , "NBCCNBBBCBHCB"
                                                               , "NBBBCNCCNBBNBNBBCHBHHBCHB"
                                                               , "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"
                                                               ]
      it "solves part a" $ do
        Day14.day14 example `shouldBe` 1588
      
      let
        (start, rules) = Day14.parse example
        expansions = Day14.makeExpansionRules rules
        s0 = Day14.makePairCounts start
        iters = iterate (Day14.bumpCounts expansions) s0
      it "solves part a the clever way" $ do
        Day14.summariseItem (iters !! 10) `shouldBe` 1588
      it "solves part b" $ do
        Day14.summariseItem (iters !! 40) `shouldBe` 2188189693529

    describe "day 15" $ do
      let
        example = "1163751742\n\
                  \1381373672\n\
                  \2136511328\n\
                  \3694931569\n\
                  \7463417111\n\
                  \1319128137\n\
                  \1359912421\n\
                  \3125421639\n\
                  \1293138521\n\
                  \2311944581" & lines
      it "pathfinds for part a" $ do
        let
          Just path = Day15.day15 example
        head path `shouldBe` (9,9,40)

      let
        eg2 = "11637517422274862853338597396444961841755517295286\n\
              \13813736722492484783351359589446246169155735727126\n\
              \21365113283247622439435873354154698446526571955763\n\
              \36949315694715142671582625378269373648937148475914\n\
              \74634171118574528222968563933317967414442817852555\n\
              \13191281372421239248353234135946434524615754563572\n\
              \13599124212461123532357223464346833457545794456865\n\
              \31254216394236532741534764385264587549637569865174\n\
              \12931385212314249632342535174345364628545647573965\n\
              \23119445813422155692453326671356443778246755488935\n\
              \22748628533385973964449618417555172952866628316397\n\
              \24924847833513595894462461691557357271266846838237\n\
              \32476224394358733541546984465265719557637682166874\n\
              \47151426715826253782693736489371484759148259586125\n\
              \85745282229685639333179674144428178525553928963666\n\
              \24212392483532341359464345246157545635726865674683\n\
              \24611235323572234643468334575457944568656815567976\n\
              \42365327415347643852645875496375698651748671976285\n\
              \23142496323425351743453646285456475739656758684176\n\
              \34221556924533266713564437782467554889357866599146\n\
              \33859739644496184175551729528666283163977739427418\n\
              \35135958944624616915573572712668468382377957949348\n\
              \43587335415469844652657195576376821668748793277985\n\
              \58262537826937364893714847591482595861259361697236\n\
              \96856393331796741444281785255539289636664139174777\n\
              \35323413594643452461575456357268656746837976785794\n\
              \35722346434683345754579445686568155679767926678187\n\
              \53476438526458754963756986517486719762859782187396\n\
              \34253517434536462854564757396567586841767869795287\n\
              \45332667135644377824675548893578665991468977611257\n\
              \44961841755517295286662831639777394274188841538529\n\
              \46246169155735727126684683823779579493488168151459\n\
              \54698446526571955763768216687487932779859814388196\n\
              \69373648937148475914825958612593616972361472718347\n\
              \17967414442817852555392896366641391747775241285888\n\
              \46434524615754563572686567468379767857948187896815\n\
              \46833457545794456865681556797679266781878137789298\n\
              \64587549637569865174867197628597821873961893298417\n\
              \45364628545647573965675868417678697952878971816398\n\
              \56443778246755488935786659914689776112579188722368\n\
              \55172952866628316397773942741888415385299952649631\n\
              \57357271266846838237795794934881681514599279262561\n\
              \65719557637682166874879327798598143881961925499217\n\
              \71484759148259586125936169723614727183472583829458\n\
              \28178525553928963666413917477752412858886352396999\n\
              \57545635726865674683797678579481878968159298917926\n\
              \57944568656815567976792667818781377892989248891319\n\
              \75698651748671976285978218739618932984172914319528\n\
              \56475739656758684176786979528789718163989182927419\n\
              \67554889357866599146897761125791887223681299833479" & lines
      it "expands the map" $ do
        let
          m0 = Day15.parse example
          m1 = Day15.parse eg2
        Day15.expand m0 `shouldBe` m1
        Day15.day15b example `shouldBe` (49,49,315)        