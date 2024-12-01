module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Control.Arrow ((***))
import Data.List (sort)

type Input = ([Int],[Int])

parser :: Parser Input
parser = unzip <$> sepEndBy1 ((,) <$> lexeme integer <*> integer) eol <* eof

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . uncurry (zipWith ((abs .) . (-))) . both sort <$> input
  printAnswer "Total distance: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 01: Historian Hysteria"
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
