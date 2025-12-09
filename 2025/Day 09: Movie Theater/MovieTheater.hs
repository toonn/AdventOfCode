module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Data.List (tails)

type Input = [(Int,Int)]

parser :: Parser Input
parser = sepEndBy1 ((,) <$> integer <* char ',' <*> integer) eol <* eof

area :: (Int, Int) -> (Int, Int) -> Int
area (x1,y1) (x2,y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = foldr (\(xy:xys) -> max . maximum . map (area xy) $ xys)
                     0
             . filter ((>= 2) . length)
             . tails
           <$> input
  printAnswer "Largest rectangle: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 09: Movie Theater"
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
