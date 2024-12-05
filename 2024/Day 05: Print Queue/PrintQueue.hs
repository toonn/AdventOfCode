module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

type Input = (IM.IntMap IS.IntSet, [[Int]])

rules :: Parser (IM.IntMap IS.IntSet)
rules = foldr (\(f,t) -> IM.insertWith (<>) f (IS.singleton t)) mempty
    <$> sepEndBy1 ((,) <$> integer <* (char '|') <*> integer) eol

updates :: Parser [[Int]]
updates = sepEndBy1 (sepBy1 integer (char ',')) eol

parser :: Parser Input
parser = (,) <$> rules <* eol <*> updates <* eof

correctOrder :: IM.IntMap IS.IntSet -> [Int] -> Bool
correctOrder rs us = foldr (\p more seen ->
                             let seen' = IS.insert p seen
                              in IS.disjoint (IM.findWithDefault mempty p rs)
                                             seen
                              && more seen'
                           )
                           (const True)
                           us
                           mempty

middle :: [a] -> a
middle as = as !! (length as `quot` 2)

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . map middle . uncurry (filter . correctOrder) <$> input
  printAnswer "Correctly-ordered middles: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 05: Print Queue"
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
