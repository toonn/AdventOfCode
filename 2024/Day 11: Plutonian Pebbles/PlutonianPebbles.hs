module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

type Input = [Int]

parser :: Parser Input
parser = some integer <* space <* eof

blink :: [Int] -> [Int]
blink = foldr (\stone ->
                let ins | stone == 0 = (1:)
                        | let st = show stone
                        , (half, 0) <- length st `quotRem` 2
                        = (read (take half st) :)
                        . (read (drop half st) :)
                        | otherwise = (stone * 2024 :)
                 in ins
              )
              mempty

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = length . nTimes 25 blink <$> input
  printAnswer "Stones after 25 blinks: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 11: Plutonian Pebbles"
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
