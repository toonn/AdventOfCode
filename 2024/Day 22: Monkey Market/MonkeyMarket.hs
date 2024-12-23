module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Control.Arrow ((&&&))
import Data.Bits (xor)

type Input = [Int]

parser :: Parser Input
parser = some (integer <* eol) <* eof

mix :: (Int, Int) -> Int
mix = uncurry xor

prune :: Int -> Int
prune = (`mod` 16777216)

step :: Int -> Int
step = prune . mix . (id &&& (* 2048))
     . prune . mix . (id &&& (`quot` 32))
     . prune . mix . (id &&& (* 64))

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . map (nTimes 2000 step) <$> input
  printAnswer "Sum of 2000th secret numbers: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 22: Monkey Market"
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
