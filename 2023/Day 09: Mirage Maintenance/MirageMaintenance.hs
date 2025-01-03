module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AoC

type Input = [[Int]]

parser :: Parser Input
parser = sepEndBy (some (signed integer)) eol <* eof

extrapolate :: [Int] -> Int
extrapolate vs | all (== 0) vs = 0
               | otherwise = let diffs = zipWith (-) (tail vs) vs
                                 extrapolatedDiff = extrapolate diffs
                                 extrapolated = last vs + extrapolatedDiff
                              in extrapolated

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . map extrapolate <$> input
  printAnswer "Sum of extrapolated values: " answer

extrapolateBackward :: [Int] -> Int
extrapolateBackward vs | all (== 0) vs = 0
                       | otherwise
                       = let diffs = zipWith (-) (tail vs) vs
                             extrapolatedDiff = extrapolateBackward diffs
                             extrapolated = head vs - extrapolatedDiff
                          in extrapolated

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = sum . map extrapolateBackward <$> input
  printAnswer "Sum of extrapolated previous values: " answer

main :: IO ()
main = do
  let day = "Day 09: Mirage Maintenance"
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
