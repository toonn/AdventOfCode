module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AoC

import Data.List (find, partition, tails)

type SpringState = ([Char], [Int])

type Input = [SpringState]

springState :: Parser SpringState
springState = do
  states <- takeWhile1P (Just "Spring state") (`elem` ".#?")
  hspace
  groupSizes <- sepBy integer (char ',')
  pure (states, groupSizes)

parser :: Parser Input
parser = sepEndBy1 springState eol <* eof

nrArrangements :: SpringState -> Int
nrArrangements (springs, [])
  | any (== '#') springs = 0 -- Any broken springs would have to be part of a
                             -- contiguous group
  | otherwise = 1
nrArrangements (springs, size:groupSizes)
  | springs' <- dropWhile (== '.') springs
  , length springs' >= size + sum groupSizes + length groupSizes
  = let recurseNr | ('?':rest) <- springs'
                  = nrArrangements (rest, size : groupSizes)
                  | otherwise
                  = 0
        (prefix, postfix) = splitAt size springs'
        nr | any (== '.') prefix
           = let rNr | (n:prefix') <- prefix
                     , n /= '#'
                     , Just prefix'' <-
                         find (all (/= '.'))
                       . (\(fine, potential) ->
                           let pF | (p:_) <- potential = [p]
                                  | otherwise = []
                            in fine <> pF
                         )
                       . span (\l -> null l || head l /= '#')
                       . tails
                       $ prefix'
                     = nrArrangements (prefix'' <> postfix, size:groupSizes)
                     | otherwise
                     = 0
              in rNr
           | (n:postfix') <- postfix
           = let reducedNr | n == '#' = 0
                           | otherwise = nrArrangements (postfix', groupSizes)
              in recurseNr + reducedNr
           | [] <- postfix, [] <- groupSizes
           = 1
     in nr
  | otherwise = 0

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . map nrArrangements <$> input
  printAnswer "Number of arrangements: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 12: Hot Springs"
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
