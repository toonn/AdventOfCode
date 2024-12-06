module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import qualified Data.Map as M
import qualified Data.Set as S

data Orientation = N | E | S | W deriving (Show)

type Input = [[Char]]

parser :: Parser Input
parser = characterGrid <* eof

step :: YX -> Orientation -> YX
step (y,x) N = (y - 1, x)
step (y,x) E = (y, x + 1)
step (y,x) S = (y + 1, x)
step (y,x) W = (y, x - 1)

turn :: Orientation -> Orientation
turn N = E
turn E = S
turn S = W
turn W = N

patrol' :: M.Map YX Char -> S.Set YX -> YX -> Orientation -> S.Set YX
patrol' layout seen p o =
  let next = step p o
      spot = M.findWithDefault ' ' next layout
      res | ' ' <- spot = seen
          | '#' <- spot = patrol' layout seen p (turn o)
          | otherwise = patrol' layout (S.insert next seen) next o
   in res


patrol :: M.Map YX Char -> S.Set YX
patrol layout =
  let start = M.foldrWithKey (\k c rest -> if c == '^' then k else rest)
                             (error "No starting position")
                             layout
   in patrol' layout (S.singleton start) start N

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = length . patrol . foldYX <$> input
  printAnswer "Distinct positions: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 06: Guard Gallivant"
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
