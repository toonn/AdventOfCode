module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import qualified Data.Map as M

type Input = (M.Map Int Int,[M.Map Int Int])

parser :: Parser Input
parser = (\(start:splitters) -> (start, splitters))
       . map ( M.fromAscList
             . map (fmap (const 1))
             . filter ((/= '.') . snd)
             . zip [0..]
             )
     <$> sepEndBy1 (takeWhile1P (Just "One of S . ^") (/= '\n')) eol
      <* eof

simulate :: Input -> (Int, M.Map Int Int)
simulate (start, splitters) = foldr
  (\splits next (count, beams) ->
    let hits = M.intersection beams splits
        misses = beams M.\\ splits
     in next ( count + length hits
             , M.unionsWith (+)
                            [ misses
                            , M.mapKeysMonotonic (subtract 1) hits
                            , M.mapKeysMonotonic (+ 1) hits
                            ]
             )
  )
  id
  splitters
  (0, start)

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = fst . simulate <$> input
  printAnswer "Number of splits: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = sum . snd . simulate <$> input
  printAnswer "Number of timelines: " answer

main :: IO ()
main = do
  let day = "Day 07: Laboratories"
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
