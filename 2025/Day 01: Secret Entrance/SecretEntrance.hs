module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

type Input = [(Char,Int)]

parser :: Parser Input
parser = sepEndBy1 ((,) <$> lexeme (oneOf "LR") <*> lexeme integer) eol <* eof

zeroes :: Input -> Int
zeroes instructions = snd
                    $ foldr (\(dir, steps) next (current, count) ->
                              let op | dir == 'L' = (-)
                                     | otherwise  = (+)
                                  current' = (current `op` steps) `mod` 100
                                  mInc | current' == 0 = (+ 1)
                                       | otherwise     = id
                               in next (current', mInc count)
                            )
                            id
                            instructions
                            (50,0)

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = zeroes <$> input
  printAnswer "Actual password: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 01: Secret Entrance"
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
