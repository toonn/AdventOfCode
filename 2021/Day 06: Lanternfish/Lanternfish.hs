module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Monoid (Sum(..))
import qualified Data.IntMap as M

import AoC

type Fish = M.IntMap Int

parser :: Parser Fish
parser = foldr (\days -> M.insertWith (const (+ 1)) days 1) M.empty
     <$> sepBy integer (char ',') <* (eol *> eof)

simulate :: Int -> Fish -> Fish
simulate 0 fish = fish
simulate steps fish = simulate (steps - 1) fish'
  where
    fish' = M.foldrWithKey (\days count fs ->
                             let (days',fs')
                                   | days == 0 = (6, M.insert 8 count fs)
                                   | otherwise = (days - 1, fs)
                             in M.insertWith (+) days' count fs'
                           )
                           M.empty
                           fish

tally :: Fish -> Int
tally = getSum . mconcat . M.foldr ((:) . Sum) []

part1 :: Parsed Fish -> IO ()
part1 input = do
  let answer = tally . simulate 80 <$> input
  printAnswer "Lanternfish after 80 days: " answer

part2 :: Parsed Fish -> IO ()
part2 input = do
  let answer = tally . simulate 256 <$> input
  printAnswer "Lanternfish after 256 days: " answer

main :: IO ()
main = do
  let day = "Day 06: Lanternfish"
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
