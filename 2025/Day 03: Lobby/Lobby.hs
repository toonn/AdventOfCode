module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Data.Char (isDigit)
import Data.List (delete)
import Data.Monoid (Sum(..))

type Input = [String]

parser :: Parser Input
parser = sepEndBy1 (takeWhile1P (Just "digit") isDigit) eol <* eof

largestJoltage :: Int -> String -> Int
largestJoltage batteries bank = read $ foldr pushdown initial candidates
  where
    (candidates, initial) = splitAt (length bank - batteries) bank

    pushdown :: Char -> String -> String
    pushdown _ [] = []
    pushdown c jolts@(j:js) | c < j = jolts
                            | otherwise = c : pushdown j js

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = getSum . foldMap (Sum . largestJoltage 2) <$> input
  printAnswer "Total joltage: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = getSum . foldMap (Sum . largestJoltage 12) <$> input
  printAnswer "Joltage from 12 batteries per bank: " answer

main :: IO ()
main = do
  let day = "Day 03: Lobby"
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
