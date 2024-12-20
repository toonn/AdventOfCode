module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Data.Bits (xor)
import Data.List (intercalate)

type Input = (Int, Int, Int, [Int])

register :: Parser Int
register = string "Register " *> oneOf "ABC" *> string ": " *> integer

program :: Parser [Int]
program = string "Program: " *> sepBy1 integer (char ',')

parser :: Parser Input
parser = (,,,) <$> register <* eol <*> register <* eol <*> register <* eol
      <* eol <*> program <* eol <* eof

adv, bxl, bst, jnz, bxc, out, bdv, cdv :: Int
[adv, bxl, bst, jnz, bxc, out, bdv, cdv] = [0..7]

combo :: Int -> Int -> Int -> Int -> Int
combo regA regB regC op | op <= 3 = op
                        | op == 4 = regA
                        | op == 5 = regB
                        | op == 6 = regC
                        | op == 7 = error "Reserved combo operand 7"

interpret :: [Int] -> Input -> [Int]
interpret p0 (regA, regB, regC, (inst:op:p))
  | inst == adv
  = interpret p0 (regA `quot` 2 ^ (combo regA regB regC op), regB, regC, p)
  | inst == bxl = interpret p0 (regA, regB `xor` op, regC, p)
  | inst == bst = interpret p0 (regA, combo regA regB regC op `mod` 8, regC, p)
  | inst == jnz = let p' | regA == 0 = p
                         | otherwise = drop op p0
                   in interpret p0 (regA, regB, regC, p')
  | inst == bxc = interpret p0 (regA, regB `xor` regC, regC, p)
  | inst == out
  = combo regA regB regC op `mod` 8 : interpret p0 (regA, regB, regC, p)
  | inst == bdv
  = interpret p0 (regA, regA `quot` 2 ^ (combo regA regB regC op), regC, p)
  | inst == cdv
  = interpret p0 (regA, regB, regA `quot` 2 ^ (combo regA regB regC op), p)
interpret _ _ = []

output :: [Int] -> String
output = intercalate "," . map show

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = output . (\(regA,regB,regC,p) -> interpret p (regA,regB,regC,p))
           <$> input
  printAnswer "Comma-separated Values: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 17: Chronospatial Computer"
  input <- readInput day parser
  putStrLn ""
  part1 input
  part2 input
  putStrLn ""
  defaultMainWith (defaultConfig { resamples = 1 }) [
      bgroup "AoC"
        [ bench "Part 1" $ nfIO (silence $ readInput day parser >>= part1)
        , bench "Part 2" $ nfIO (silence $ readInput day parser >>= part2)
        ]
    ]
