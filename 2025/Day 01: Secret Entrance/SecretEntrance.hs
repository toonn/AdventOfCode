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

zeroes :: (Char -> Int -> Int -> Int -> Int) -> Input -> Int
zeroes increment instructions
  = snd
  $ foldr (\(dir, steps) next (current, count) ->
            let op | dir == 'L' = (-)
                   | otherwise  = (+)
                new = (current `op` steps) `mod` 100
             in next (new, count + increment dir steps current new)
          )
          id
          instructions
          (50,0)

zeroReaches :: a -> b -> c -> Int -> Int
zeroReaches _ _ _ new | new == 0  = 1
                      | otherwise = 0

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = zeroes zeroReaches <$> input
  printAnswer "Actual password: " answer


zeroPasses :: Char -> Int -> Int -> Int -> Int
zeroPasses dir steps current new = steps `quot` 100 + passZero
  where passZero | steps == 0
                || current == 0
                || (dir == 'L' && new /= 0 && new < current)
                || (dir == 'R' && new > current)
                 = 0
                 | otherwise = 1

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = zeroes zeroPasses <$> input
  printAnswer "Every zero pass: " answer

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
