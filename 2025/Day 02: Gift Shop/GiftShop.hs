module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Data.List (nub)
import Data.Monoid (Sum(..))

type Input = [(Int,Int)]

parser :: Parser Input
parser = sepBy1 ((,) <$> lexeme integer <*> (char '-' *> lexeme integer))
                (char ',')
      <* eol <* eof

add :: Num n => n -> Sum n -> Sum n
add = (<>) . Sum

-- Too low:  15387968268
-- Too high: 16793893635
part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = getSum . foldMap (repeatedNTimes add 2) <$> input
  printAnswer "Sum of invalid IDs: " answer

repeatedNTimes :: Monoid m => (Int -> m -> m) -> Int -> (Int, Int) -> m
repeatedNTimes prepend repeats (low,high) = go prepend places lowestCandidate
  where
    lowString = show low
    (part, bias) = length lowString `quotRem` repeats
    (places, repeatString)
      | bias == 0 = (10 ^ part, take part lowString)
      | otherwise = (10 ^ (part + 1), '1' : replicate part '0')
    lowestCandidate = read repeatString

    go :: Monoid m => (Int -> m -> m) -> Int -> Int -> m
    go prepend p n | repeatN > high = mempty
                   | otherwise = prependInvalid (go prepend (mInc p) (n + 1))
      where
        repeatN = getSum $ foldMap (Sum . (n *) . (p^)) [0..repeats-1]
        mInc | n + 1 == p = (* 10)
             | otherwise  = id
        prependInvalid | low <= repeatN, repeatN <= high = prepend repeatN
                       | otherwise                       = id

repeatedAnyTimes :: (Int, Int) -> [Int]
repeatedAnyTimes bounds = nub
                        $ foldMap (\r -> repeatedNTimes (:) r bounds)
                                  [2..maxLen]
  where maxLen = uncurry max (both (length . show) bounds)

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = sum . foldMap repeatedAnyTimes <$> input
  printAnswer "Sum of all invalid IDs: " answer

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
