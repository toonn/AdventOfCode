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

findAntinodes :: (YX -> YX -> S.Set YX) -> M.Map Char (S.Set YX) -> S.Set YX
findAntinodes antinodeSet = foldMap (\antennas ->
                                      foldr (\a more seen ->
                                              foldMap (antinodeSet a) seen
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
               . findAntinodes antinodes
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

bounds :: M.Map YX a -> YX
bounds = (\ks -> (S.findMax (S.map fst ks), S.findMax (S.map snd ks)))
       . M.keysSet

resonantAntinodes :: YX -> YX -> YX -> S.Set YX
resonantAntinodes (yM,xM) (y1,x1) (y2,x2)
  | let dy = y1 - y2, let dx = x1 - x2 =
    let go :: (Int -> Int -> Int) -> YX -> S.Set YX
        go op (y,x) | y < 0 || y > yM || x < 0 || x > xM = mempty
                    | otherwise = S.insert (y,x) (go op (y `op` dy, x `op` dx))
     in go (+) (y1,x1) <> go (-) (y1,x1)

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = length
             . (\grid ->
                 findAntinodes (resonantAntinodes (bounds grid))
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
  printAnswer "Resonant antinodes: " answer

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
