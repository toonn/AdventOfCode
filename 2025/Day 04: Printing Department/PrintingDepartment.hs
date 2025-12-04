module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import qualified Data.Map as M
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

dropAllReachable :: M.Map YX Int -> Int
dropAllReachable = dropReachable 0
  where
    dropReachable :: Int -> M.Map YX Int -> Int
    dropReachable c m
      = let (reachable, rolls) = M.partition (< 4) m
            r | null reachable = c
              | otherwise
              = dropReachable (c + length reachable)
                              (foldr ( flip (foldr (M.adjust (subtract 1)))
                                     . deltaNeighbors twoDDeltas
                                     )
                                     rolls
                                     (M.keys reachable)
                              )
         in r

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = dropAllReachable
             . (\rolls -> M.fromSet ( length
                                    . S.intersection rolls
                                    . S.fromList
                                    . deltaNeighbors twoDDeltas
                                    )
                                    rolls
               ) <$> input
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
