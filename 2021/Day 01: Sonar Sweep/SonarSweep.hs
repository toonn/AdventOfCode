module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AoC

type Measurement = Int

depthMeasurements :: Parser [Measurement]
depthMeasurements = manyTill (integer >>= \i -> optional eol >> pure i) eof

increases :: [Measurement] -> Int
increases measurements = foldr (\d next (d', c) -> case d' < d of
                                        True -> next (d, c + 1)
                                        False -> next (d, c)
                                      ) snd measurements (head measurements, 0)

part1 :: Parsed [Measurement] -> IO ()
part1 input = do
  let answer = increases <$> input
  printAnswer "Measurement increases: " answer

part2 :: Parsed [Measurement] -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "Not an answer: " answer

main :: IO ()
main = do
  let day = "Day 01: Sonar Sweep"
  let parser = depthMeasurements
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
