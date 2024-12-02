module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

type Input = [[Int]]

report :: Parser [Int]
report = some integer

parser :: Parser Input
parser = sepEndBy1 report eol <* eof

isSafe :: [Int] -> Bool
isSafe r = foldr (\lvlD more sgn ->
                   (signum lvlD + sgn /= 0)
                   && (abs lvlD >= 1 && abs lvlD <= 3)
                   && more (signum lvlD)
                 )
                 (const True)
                 (zipWith (-) r (drop 1 r))
                 0

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = length . filter id . map isSafe <$> input
  printAnswer "Safe reports: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 02: Red-Nosed Reports"
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
