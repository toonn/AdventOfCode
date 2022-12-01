module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AoC

type Calories = Int

calorieList :: Parser [Calories]
calorieList = many (integer <* eol)

parser :: Parser [[Calories]]
parser = manyTill (calorieList <* optional eol) eof

mostCarried :: [[Calories]] -> Calories
mostCarried = maximum . map sum

part1 :: Parsed [[Calories]] -> IO ()
part1 input = do
  let answer = mostCarried <$> input
  printAnswer "Most calories carried: " answer

part2 :: Parsed [[Calories]] -> IO ()
part2 input = do
  let answer = const "P" <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 01: Calorie Counting"
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
