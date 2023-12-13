module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Data.List (findIndex, inits, tails, transpose)

type Pattern = [[Char]]

type Input = [Pattern]

pattern :: Parser Pattern
pattern = sepEndBy1 (takeWhile1P (Just "Ash or Rocks") (`elem` ".#")) eol

parser :: Parser Input
parser = sepBy1 pattern eol <* eof

reflect :: Eq a => ([[a]] -> [[a]] -> Bool) -> [[a]] -> Int
reflect equals rows =
  let reflectRow =
        findIndex id
                  (zipWith (\init tail ->
                             let lI = length init
                                 lT = length tail
                                 (init', tail')
                                   | lI <= lT  = (init, take lI tail)
                                   | otherwise = (drop (lI - lT) init, tail)
                                 reflected | null tail'
                                           = False
                                           | otherwise
                                           = init' `equals` reverse tail'
                              in reflected
                           )
                           (inits rows)
                           (tails rows)
                  )
      cols = transpose rows
      reflectCol =
        findIndex id
                  (zipWith (\init tail ->
                             let lI = length init
                                 lT = length tail
                                 (init', tail')
                                   | lI <= lT  = (init, take lI tail)
                                   | otherwise = (drop (lI - lT) init, tail)
                                 reflected | null tail'
                                           = False
                                           | otherwise
                                           = init' `equals` reverse tail'
                              in reflected
                           )
                           (inits cols)
                           (tails cols)
                  )
      summaryNr | Just r <- reflectRow = 100 * r
                | Just c <- reflectCol = c
   in summaryNr

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . map (reflect (==)) <$> input
  printAnswer "Summary: " answer

(##) :: Eq a => [[a]] -> [[a]] -> Bool
[] ## []         = False
[] ## _          = False
_  ## []         = False
(a:as) ## (b:bs) | a == b
                 = as ## bs
                 | a `hamming` b == 1
                 = as == bs
                 | otherwise
                 = False

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = sum . map (reflect (##)) <$> input
  printAnswer "Summary after fixing smudges: " answer

main :: IO ()
main = do
  let day = "Day 13: Point of Incidence"
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
