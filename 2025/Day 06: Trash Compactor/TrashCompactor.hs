module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

type Input = ([[Int]],[Char])

parser :: Parser Input
parser = (,)
     <$> sepEndBy1 (hspace *> some integer) eol
     <*> (hspace *> some (lexeme (anySingleBut '\n')) <* eol)
      <* eof

solve :: Input -> [Int]
solve (problems, ops) = foldr (\numbers intermediate ->
                                zipWith3 (\op a b -> a `op` b)
                                         opFs
                                         numbers
                                         intermediate
                              )
                              units
                              problems
  where
    (opFs, units) = unzip
                  . map (\op -> let unit | op == '*' = ((*), 1)
                                         | otherwise = ((+), 0)
                                 in unit
                        )
                  $ ops

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . solve <$> input
  printAnswer "Grand total: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

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
