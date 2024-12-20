module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import qualified Data.Set as S

type Input = [YX]

coord :: Parser YX
coord = flip (,) <$> integer <* char ',' <*> integer

parser :: Parser Input
parser = sepEndBy1 coord eol <* eof

neighbors :: YX -> S.Set YX -> YX -> [YX]
neighbors (bY,bX) corrupt (y,x) = [ (y', x') | dy <- [-1,0,1]
                                             , dx <- [-1,0,1]
                                             , abs dy /= abs dx
                                             , let y' = y + dy
                                             , let x' = x + dx
                                             , y' >= 0
                                             , y' <= bY
                                             , x' >= 0
                                             , x' <= bX
                                             , S.notMember (y', x') corrupt
                                  ]

minimalSafeSteps :: YX -> YX -> Int -> [YX] -> Int
minimalSafeSteps start end n bytes
  = aStar (neighbors end (S.fromList (take n bytes)))
          manhattan
          (manhattan end)
          (== end)
          start

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = minimalSafeSteps (0,0) (70,70) 1024 <$> input
  printAnswer "Minimum steps: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 18: RAM Run"
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
