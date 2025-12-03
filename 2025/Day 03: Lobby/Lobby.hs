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

largestJoltage :: String -> Int
largestJoltage bank
  = read
  $ foldr (\candidate jolts ->
            foldr (\j next mC ->
                    case mC of
                      Just c | c >= j -> c : next (Just j)
                      _               -> j : next Nothing
                  )
                  (const [])
                  jolts
                  (Just candidate)
          )
          initial
          candidates
  where
    (candidates, initial) = splitAt (length bank - 2) bank

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = getSum . foldMap (Sum . largestJoltage) <$> input
  printAnswer "Total joltage: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

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
