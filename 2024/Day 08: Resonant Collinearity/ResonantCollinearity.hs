module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import qualified Data.Map as M
import qualified Data.Set as S

type Input = [[Char]]

parser :: Parser Input
parser = characterGrid <* eof

antinodes :: YX -> YX -> S.Set YX
antinodes (y1,x1) (y2,x2) | let dy = y1 - y2, let dx = x1 - x2
                          = S.fromList [(y1 + dy, x1 + dx), (y2 - dy, x2 - dx)]

findAntinodes :: M.Map Char (S.Set YX) -> S.Set YX
findAntinodes = foldMap (\antennas ->
                          foldr (\a more seen ->
                                  foldMap (antinodes a) seen
                               <> more (a:seen)
                                )
                                (const mempty)
                                antennas
                                []
                        )

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = length
             . (\grid ->
                 S.intersection (M.keysSet grid)
               . findAntinodes
               . M.foldrWithKey (\k v ->
                                  if v == '.'
                                  then id
                                  else M.insertWith (<>) v (S.singleton k)
                                )
                                mempty
               $ grid
               )
             . foldYX
           <$> input
  printAnswer "Antinodes: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 08: Resonant Collinearity"
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
