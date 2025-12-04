module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import qualified Data.Map as M
import Prelude hiding (filter)
import qualified Data.Set as S
import Witherable (filter)

type Input = M.Map YX Char

parser :: Parser Input
parser = foldYX <$> characterGrid <* eof

accessible :: M.Map YX Char -> [YX]
accessible grid = filter ( (< 4)
                         . length
                         . M.restrictKeys rolls
                         . S.fromList
                         . deltaNeighbors twoDDeltas
                         )
                . M.keys
                $ rolls
  where rolls = filter (== '@') grid

-- Too low:  338
part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = length . accessible <$> input
  printAnswer "Accessible rolls: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 04: Printing Department"
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
