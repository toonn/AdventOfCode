module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

type Input = [(Int,[Int])]

parser :: Parser Input
parser = sepEndBy1 ((,) <$> integer <* string ": " <*> some integer) eol <* eof

validEquation :: (Int, [Int]) -> Bool
validEquation (r,is) = foldr (\i more running ->
                               more (i * running) || more (i + running)
                             )
                             (== r)
                             is
                             0

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . map fst . filter validEquation <$> input
  printAnswer "Total calibration: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 07: Bridge Repair"
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
