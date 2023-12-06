module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AoC

type Input = [(Int, Int)]

times :: Parser [Int]
times = lexeme (string "Time:") *> many integer

distances :: Parser [Int]
distances = lexeme (string "Distance:") *> many integer

parser :: Parser Input
parser = zip <$> (times <* eol) <*> (distances <* eol) <* eof

nrWays :: (Int, Int) -> Int
nrWays (time, distance) =
  let squaredHalfDistance = time ^ 2 - 4 * distance
      halfDistance = sqrt
                   . fromIntegral
                   $ squaredHalfDistance
      solutionL = floor
                . (+ 1)
                $ (fromIntegral time - halfDistance) / 2
      solutionR = ceiling
                . (subtract 1)
                $ (fromIntegral time + halfDistance) / 2
      ways | squaredHalfDistance <= 0 = 0
           | otherwise = solutionR - solutionL + 1
   in ways

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = product . map nrWays <$> input
  printAnswer "Product of number of ways to win: " answer

fixKerning :: Input -> (Int, Int)
fixKerning = (\(t,d) -> (read t, read d))
           . foldr (\(t,d) (t',d') -> (show t <> t', show d <> d'))
                   ("","")

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = nrWays . fixKerning <$> input
  printAnswer "Ways to win in one long race: " answer

main :: IO ()
main = do
  let day = "Day 06: Wait For It"
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
