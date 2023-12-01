module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Char (digitToInt, isAlphaNum, isDigit)

import AoC

type Line = String
type Input = [Line]

line :: Parser Line
line = takeWhileP (Just "character") isAlphaNum

parser :: Parser Input
parser = manyTill (line <* optional eol) eof

calibrationValues :: Line -> Int
calibrationValues l = (\(t,u) -> 10*t + u)
                    . foldr (\c more (first, last) ->
                              let d = digitToInt c
                                  fl' | isDigit c = (first . const d,  d)
                                      | otherwise = (first, last)
                               in more fl'
                            )
                            (\(first,last) -> (first 0, last))
                            l
                    $ (id,0)

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . map calibrationValues <$> input
  printAnswer "Sum of calibration values: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const "P" <$> input
  printAnswer ": " answer

main :: IO ()
main = do
  let day = "Day 01: Trebuchet?!"
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
