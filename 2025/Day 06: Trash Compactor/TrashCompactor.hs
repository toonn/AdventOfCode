module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Control.Arrow ((&&&), (***))
import Data.List (transpose)
import Data.List.Split (splitWhen)

type Input = ([String],[Char])

parser :: Parser Input
parser = (init &&& filter (/= ' ') . last)
     <$> sepEndBy1 (takeWhile1P (Just "any character") (/= '\n')) eol
      <* eof

solve :: ([[Int]],[Char]) -> [Int]
solve (problems, ops) = zipWith foldr1 opFs problems
  where
    opFs = map (\op -> let opF | op == '*' = (*)
                               | otherwise = (+)
                        in opF
               )
         $ ops

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . solve . (transpose . map (map read . words) *** id)
           <$> input
  printAnswer "Grand total: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = sum
             . solve
             . (map (map read) . splitWhen (all (== ' ')) . transpose *** id)
           <$> input
  printAnswer "Cephalopod math total: " answer

main :: IO ()
main = do
  let day = "Day 06: Trash Compactor"
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
