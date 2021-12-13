module Main where

import System.Environment
import Control.Monad (forM_)

import Lib
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day20
import Day21
import Day22
import Day23
import Day24
import Day25

main :: IO ()
main = do
  args <- getArgs
  let action = case args !! 0 of
                 "day1" -> show . day1
                 "day1b" -> show . day1b
                 "day2" -> show . day2
                 "day2b" -> show . day2b
                 "day3" -> show . day3
                 "day3b" -> show . day3b
                 "day4" -> show . day4
                 "day4b" -> show . day4b
                 "day5" -> show . day5
                 "day5b" -> show . day5b
                 "day6" -> show . day6
                 "day6b" -> show . day6b
                 "day7" -> show . day7
                 "day7b" -> show . day7b
                 "day8" -> show . day8
                 "day8b" -> show . day8b
                 "day9" -> show . day9
                 "day9b" -> show . day9b
                 "day10" -> show . day10
                 "day10b" -> show . day10b
                 "day11" -> show . day11
                 "day11b" -> show . day11b
                 "day12" -> show . day12
                 "day12b" -> show . day12b
                 "day13" -> show . day13
                 "day13b" -> day13b
                 "day14" -> show . day14
                 "day14b" -> show . day14b
                 "day15" -> show . day15
                 "day15b" -> show . day15b
                 "day16" -> show . day16
                 "day16b" -> show . day16b
                 "day17" -> show . day17
                 "day17b" -> show . day17b
                 "day18" -> show . day18
                 "day18b" -> show . day18b
                 "day19" -> show . day19
                 "day19b" -> show . day19b
                 "day20" -> show . day20
                 "day20b" -> show . day20b
                 "day21" -> show . day21
                 "day21b" -> show . day21b
                 "day22" -> show . day22
                 "day22b" -> show . day22b
                 "day23" -> show . day23
                 "day23b" -> show . day23b
                 "day24" -> show . day24
                 "day24b" -> show . day24b
                 "day25" -> show . day25
                 "day25b" -> show . day25b
      source = args !! 1
  ls <- loadLines source
  putStrLn (action ls)
