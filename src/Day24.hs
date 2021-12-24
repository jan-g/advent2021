module Day24 where

import Data.Function ((&))
import Data.Functor (($>))
import Data.List.Split
import Data.List as L
import Data.Array as A
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe (catMaybes, isJust, fromJust, fromMaybe, mapMaybe)
import Text.ParserCombinators.ReadP as P
import Numeric (readInt)
import Data.Bits ((.&.), (.|.))

import Debug.Trace (trace)

import Lib


{-
--- Day 24: Arithmetic Logic Unit ---

Magic smoke starts leaking from the submarine's arithmetic logic unit (ALU). Without the ability to perform basic arithmetic and logic functions, the submarine can't produce cool patterns with its Christmas lights!

It also can't navigate. Or run the oxygen system.

Don't worry, though - you probably have enough oxygen left to give you enough time to build a new ALU.

The ALU is a four-dimensional processing unit: it has integer variables w, x, y, and z. These variables all start with the value 0. The ALU also supports six instructions:

    inp a - Read an input value and write it to variable a.
    add a b - Add the value of a to the value of b, then store the result in variable a.
    mul a b - Multiply the value of a by the value of b, then store the result in variable a.
    div a b - Divide the value of a by the value of b, truncate the result to an integer, then store the result in variable a. (Here, "truncate" means to round the value toward zero.)
    mod a b - Divide the value of a by the value of b, then store the remainder in variable a. (This is also called the modulo operation.)
    eql a b - If the value of a and b are equal, then store the value 1 in variable a. Otherwise, store the value 0 in variable a.

In all of these instructions, a and b are placeholders; a will always be the variable where the result of the operation is stored (one of w, x, y, or z), while b can be either a variable or a number. Numbers can be positive or negative, but will always be integers.

The ALU has no jump instructions; in an ALU program, every instruction is run exactly once in order from top to bottom. The program halts after the last instruction has finished executing.

(Program authors should be especially cautious; attempting to execute div with b=0 or attempting to execute mod with a<0 or b<=0 will cause the program to crash and might even damage the ALU. These operations are never intended in any serious ALU program.)

For example, here is an ALU program which takes an input number, negates it, and stores it in x:

inp x
mul x -1

Here is an ALU program which takes two input numbers, then sets z to 1 if the second input number is three times larger than the first input number, or sets z to 0 otherwise:

inp z
inp x
mul z 3
eql z x

Here is an ALU program which takes a non-negative integer as input, converts it into binary, and stores the lowest (1's) bit in z, the second-lowest (2's) bit in y, the third-lowest (4's) bit in x, and the fourth-lowest (8's) bit in w:

inp w
add z w
mod z 2
div w 2
add y w
mod y 2
div w 2
add x w
mod x 2
div w 2
mod w 2

Once you have built a replacement ALU, you can install it in the submarine, which will immediately resume what it was doing when the ALU failed: validating the submarine's model number. To do this, the ALU will run the MOdel Number Automatic Detector program (MONAD, your puzzle input).

Submarine model numbers are always fourteen-digit numbers consisting only of digits 1 through 9. The digit 0 cannot appear in a model number.

When MONAD checks a hypothetical fourteen-digit model number, it uses fourteen separate inp instructions, each expecting a single digit of the model number in order of most to least significant. (So, to check the model number 13579246899999, you would give 1 to the first inp instruction, 3 to the second inp instruction, 5 to the third inp instruction, and so on.) This means that when operating MONAD, each input instruction should only ever be given an integer value of at least 1 and at most 9.

Then, after MONAD has finished running all of its instructions, it will indicate that the model number was valid by leaving a 0 in variable z. However, if the model number was invalid, it will leave some other non-zero value in z.

MONAD imposes additional, mysterious restrictions on model numbers, and legend says the last copy of the MONAD documentation was eaten by a tanuki. You'll need to figure out what MONAD does some other way.

To enable as many submarine features as possible, find the largest valid fourteen-digit model number that contains no 0 digits. What is the largest model number accepted by MONAD?

To begin, get your puzzle input.
-}

data Instr = Inp Reg
           | Add Reg Source
           | Mul Reg Source
           | Div Reg Source  {- truncates towards zero -}
           | Mod Reg Source
           | Eql Reg Source
  deriving (Show, Eq)

data Reg = W | X | Y | Z deriving (Show, Eq, Ord)
data Source = R Reg | I Integer deriving (Show, Eq)

parse :: [String] -> [Instr]
parse ls = ls
         & mapMaybe (quickParse parseInstr)

parseInstr :: ReadP Instr
parseInstr =
  (Inp <$> (string "inp " *> parseReg <* eof)) +++
  (Add <$> (string "add " *> parseReg) <*> (char ' ' *> parseSource <* eof)) +++
  (Mul <$> (string "mul " *> parseReg) <*> (char ' ' *> parseSource <* eof)) +++
  (Div <$> (string "div " *> parseReg) <*> (char ' ' *> parseSource <* eof)) +++
  (Mod <$> (string "mod " *> parseReg) <*> (char ' ' *> parseSource <* eof)) +++
  (Eql <$> (string "eql " *> parseReg) <*> (char ' ' *> parseSource <* eof))

parseReg :: ReadP Reg
parseReg =
  (char 'w' $> W) +++
  (char 'x' $> X) +++
  (char 'y' $> Y) +++
  (char 'z' $> Z)

parseSource :: ReadP Source
parseSource = (R <$> parseReg) +++ (I <$> intParser)

run :: [Instr] -> Map.Map Reg Integer -> [Integer] -> Map.Map Reg Integer
run [] rs _ = rs
run (ins:rest) rs is =
  case ins of
    Inp r -> run rest (Map.insert r (head is) rs) (tail is)
    Add r s -> run rest (Map.insert r (rs Map.! r + eval s) rs) is
    Mul r s -> run rest (Map.insert r (rs Map.! r * eval s) rs) is
    Div r s -> run rest (Map.insert r (rs Map.! r `quot` eval s) rs) is
    Mod r s -> run rest (Map.insert r (rs Map.! r `mod` eval s) rs) is
    Eql r s -> run rest (Map.insert r (if rs Map.! r == eval s then 1 else 0) rs) is
  where
  eval (R r) = rs Map.! r
  eval (I i) = i

reg0 = Map.fromList [(W, 0), (X, 0), (Y, 0), (Z, 0)]
regZ z = Map.fromList [(W, 0), (X, 0), (Y, 0), (Z, z)]

day24 ls =
  let
    is = parse ls
    is' = splitOn [Inp W] is & tail & map (Inp W:)  -- drop the first empty
    allPaths = foldl step (Map.singleton 0 []) is'
  in
    allPaths Map.! 0

step :: Map.Map Integer [Integer]   -- the value in Z sequence that's led us to this value
     -> Prog                        -- the next program chunk (this resets W, X and Y before use)
     -> Map.Map Integer [Integer]   -- the output values of Z and the largeest input sequence that give it
step paths prog =
  let
    outputs = do
      (z, path) <- paths & Map.toList
      pure $ trial path prog (regZ z)   -- this gives us a sequence of {Z -> [path]}
  in
    trace ("at the next step I start with " ++ show (Map.size paths) ++ " options") $
    Map.unionsWith max outputs

-- given an input set of registers, and the prefix that led to them, run a snippet with all possible inputs.
-- return a map of register Z outputs -> largest input sequence that produced that result
type Registers = Map.Map Reg Integer
type Prog = [Instr]

trial :: [Integer] -> Prog -> Registers -> Map.Map Integer [Integer]
trial prefix prog rs =
  Map.fromList $ do
      n <- [1..9]
      pure (run prog rs [n] Map.! Z, prefix ++ [n])


{-
--- Part Two ---

As the submarine starts booting up things like the Retro Encabulator, you realize that maybe you don't need all these submarine features after all.

What is the smallest model number accepted by MONAD?
-}

day24b ls =
  let
    is = parse ls
    is' = splitOn [Inp W] is & tail & map (Inp W:)  -- drop the first empty
    allPaths = foldl step' (Map.singleton 0 []) is'
  in
    allPaths Map.! 0

step' :: Map.Map Integer [Integer]   -- the value in Z sequence that's led us to this value
     -> Prog                        -- the next program chunk (this resets W, X and Y before use)
     -> Map.Map Integer [Integer]   -- the output values of Z and the largeest input sequence that give it
step' paths prog =
  let
    outputs = do
      (z, path) <- paths & Map.toList
      pure $ trial' path prog (regZ z)   -- this gives us a sequence of {Z -> [path]}
  in
    trace ("at the next step I start with " ++ show (Map.size paths) ++ " options") $
    Map.unionsWith min outputs

-- given an input set of registers, and the prefix that led to them, run a snippet with all possible inputs.
-- return a map of register Z outputs -> largest input sequence that produced that result

trial' :: [Integer] -> Prog -> Registers -> Map.Map Integer [Integer]
trial' prefix prog rs =
  Map.fromList $ do
      n <- [9,8..1]
      pure (run prog rs [n] Map.! Z, prefix ++ [n])
