module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import qualified Data.IntegerInterval as II
import qualified Data.IntervalSet as IS

type Input = (IS.IntervalSet Int,[Int])

parser :: Parser Input
parser = (,)
     <$> IS.fromList
       . map (II.toInterval . uncurry (II.<=..<=) . both II.Finite)
     <$> sepEndBy1 ((,) <$> lexeme integer <* anySingle <*> lexeme integer) eol
      <* eol
     <*> sepEndBy1 (lexeme integer) eol
      <* eof

fresh :: Ord a => IS.IntervalSet a -> a -> Bool
fresh = flip IS.member

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = length . uncurry (filter . fresh) <$> input
  printAnswer "Fresh ingredients: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 05: Cafeteria"
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
