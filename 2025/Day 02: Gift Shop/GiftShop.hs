module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Data.Monoid (Sum(..))

type Input = [(Int,Int)]

parser :: Parser Input
parser = sepBy1 ((,) <$> lexeme integer <*> (char '-' *> lexeme integer))
                (char ',')
      <* eol <* eof

repeatedTwice :: Monoid m => (Int -> m -> m) -> (Int, Int) -> m
repeatedTwice prepend (low,high) = go prepend places lowestCandidate
  where
    lowString = show low
    (half, bias) = length lowString `quotRem` 2
    (places, repeatString)
      | bias == 0 = (10 ^ half, take half lowString)
      | bias == 1 = (10 ^ (half + 1), '1' : replicate half '0')
    lowestCandidate = read repeatString

    go :: Monoid m => (Int -> m -> m) -> Int -> Int -> m
    go prepend p n | repeatN > high = mempty
                   | otherwise = prependInvalid (go prepend (mInc p) (n + 1))
      where
        repeatN = n * p + n
        mInc | n + 1 == p = (* 10)
             | otherwise  = id
        prependInvalid | low <= repeatN, repeatN <= high = prepend repeatN
                       | otherwise                       = id

-- Too low:  15387968268
-- Too high: 16793893635
part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = getSum . foldMap (repeatedTwice ((<>) . Sum)) <$> input
  printAnswer "Sum of invalid IDs: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 02: Gift Shop"
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
