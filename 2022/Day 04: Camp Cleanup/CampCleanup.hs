module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Set as S

import AoC

type Range = S.Set Int
type Pair = (Range, Range)

range :: Parser Range
range = do
  l <- integer
  char '-'
  r <- integer
  pure (S.fromList [l..r])

pair :: Parser Pair
pair = do
  l <- range
  char ','
  r <- range
  pure (l, r)

parser :: Parser [Pair]
parser = manyTill (pair <* eol) eof

fullyContained :: Pair -> Bool
fullyContained (l, r) = l `S.isSubsetOf` r || r `S.isSubsetOf` l

nrFullyContained :: [Pair] -> Int
nrFullyContained = length . filter id . map fullyContained

part1 :: Parsed [Pair] -> IO ()
part1 input = do
  let answer = nrFullyContained <$> input
  printAnswer "Fully contained ranges: " answer

nrOverlaps :: [Pair] -> Int
nrOverlaps = length . filter not . map (uncurry S.disjoint)

part2 :: Parsed [Pair] -> IO ()
part2 input = do
  let answer = nrOverlaps <$> input
  printAnswer "Overlapping ranges: " answer

main :: IO ()
main = do
  let day = "Day 04: Camp Cleanup"
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
