module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Data.List (unfoldr)
import Data.Foldable (fold)
import qualified Data.Map as M
import Data.Monoid (Sum(..))
import qualified Data.Set as S

type Input = S.Set YX

parser :: Parser Input
parser = M.keysSet . M.filter (== '@') . foldYX <$> characterGrid <* eof

accessible :: S.Set YX -> S.Set YX
accessible rolls = S.filter ( (< 4)
                            . length
                            . S.intersection rolls
                            . S.fromList
                            . deltaNeighbors twoDDeltas
                            )
                 $ rolls

-- Too low:  338
part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = length . accessible <$> input
  printAnswer "Accessible rolls: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = getSum
             . fold
             . unfoldr (\rolls ->
                         let as = accessible rolls
                             next | null as = Nothing
                                  | otherwise
                                  = Just (Sum (length as)
                                         , S.difference rolls as
                                         )
                          in next
                       )
           <$> input
  printAnswer "Removable rolls: " answer

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
