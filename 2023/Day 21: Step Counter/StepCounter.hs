module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import qualified Data.Map as M
import qualified Data.PQueue.Prio.Max as PQ
import qualified Data.Set as S

type Gardens = M.Map YX Char

type Input = [[Char]]

parser :: Parser Input
parser = characterGrid <* eof

findStart :: Gardens -> YX
findStart = M.foldrWithKey (\yx p next -> if p == 'S' then yx else next)
                           (error "No starting position found!")

neighbors :: Gardens -> YX -> S.Set YX
neighbors gardens (y,x) =
    S.fromAscList
  . filter (\p -> M.findWithDefault '#' p gardens /= '#')
  $ [(y + dy, x + dx) | dy <- [-1..1]
                      , dx <- [-1..1]
                      , abs dy /= abs dx
    ]

reach :: Int -> Gardens -> Int
reach steps gardens = length
                    . snd
                    $ go (PQ.singleton steps (findStart gardens))
                         (S.singleton (findStart gardens), mempty)
  where
    go candidates (seen,reachable)
      | null candidates = (seen, reachable)
      | ((steps, yx), rest) <- PQ.deleteFindMax candidates
      = let reachable' | even steps = S.insert yx reachable
                       | otherwise = reachable
            ns = S.filter (`S.notMember` seen) (neighbors gardens yx)
            candidates' | steps == 0 = rest
                        | otherwise = S.foldr (PQ.insert (steps - 1)) rest ns
         in go candidates' (seen <> ns, reachable')

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = reach 64 . foldYX <$> input
  printAnswer "Reachable plots in 64 steps: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 21: Step Counter"
  input <- readInput day parser
  putStrLn ""
  part1 input
  part2 input
  putStrLn ""
  defaultMain [
      bgroup "AoC"
        [ bench "Part 1" $ nfIO (silence $ readInput day parser >>= part1)
        , bench "Part 2" $ nfIO (silence $ readInput day parser >>= part2)
        ]
    ]
