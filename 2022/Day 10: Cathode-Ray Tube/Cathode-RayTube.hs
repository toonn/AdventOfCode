module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.IntMap as IM
import Data.Maybe (fromJust)

import AoC

data Instruction = NoOp | AddX Int deriving Show
type RegisterHistory = IM.IntMap Int
type Cycle = Int

type Input = [Instruction]

instruction :: Parser Instruction
instruction = (string "noop" *> pure NoOp)
          <|> (AddX <$> (lexeme (string "addx") *> signed integer))

parser :: Parser Input
parser = manyTill (instruction <* eol) eof

value :: Cycle -> RegisterHistory -> Int
value c = snd . fromJust . IM.lookupLE c

interpret :: Instruction -> (Cycle, RegisterHistory) -> (Cycle, RegisterHistory)
interpret NoOp     (c, rH) = (c + 1, rH)
interpret (AddX v) (c, rH)
  | let c' = c + 2 = (c', IM.insert (c' + 1) (value c' rH + v) rH)

signalStrengths :: [Instruction] -> [Int]
signalStrengths instructions
  = [c * (value c registerHistory) | c <- [20,60,100,140,180,220]]
  where registerHistory = foldr (\i more history ->
                                  more (interpret i history)
                                )
                                snd
                                instructions
                                (0, IM.singleton 1 1)

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . signalStrengths <$> input
  printAnswer "Sum of the six signal strengths: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const "P" <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 10: Cathode-Ray Tube"
  input <- readInput day parser
  putStrLn ""
  part1 input
  part2 input
  putStrLn ""
  defaultMain [
      bgroup "AoC"
        [ bench "Part 1" $ nfIO (silence $ readInput day parser >>= part1)
        , bench "Part 2" $ nfIO (silence $ readInput day parser >>= part2)
        ]
    ]
