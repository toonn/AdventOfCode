module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Control.Arrow ((***))
import Control.Monad (guard)
import Data.Bool (bool)

type Input = [[[Char]]]

parser :: Parser Input
parser = sepEndBy1 characterGrid eol <* eof

locksNKeys :: Input -> ([[Int]],[[Int]])
locksNKeys = foldr (\rows ->
                     let isLock = foldr (const . all (== '#')) False rows
                         ins | isLock = (*** id) . (:)
                             | otherwise = (id ***) . (:)
                      in ins $ foldr (zipWith (+) . map (bool 0 1 . (== '#')))
                                     (repeat (-1))
                                     rows
                   )
                   (mempty,mempty)

fitting :: ([[Int]],[[Int]]) -> [([Int],[Int])]
fitting (locks,keys) = do
  lock <- locks
  key <- keys
  guard $ all (<= 5) (zipWith (+) lock key)
  pure (lock,key)

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = length . fitting . locksNKeys <$> input
  printAnswer "Unique lock/key pairs: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 25: Code Chronicle"
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
