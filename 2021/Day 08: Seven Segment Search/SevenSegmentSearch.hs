module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AoC

type Digit = String
type SignalPattern = ([Digit], [Digit])

digit :: Parser Digit
digit = lexeme (many (satisfy (`elem` "abcdefg")))

signalPattern :: Parser SignalPattern
signalPattern = do
  patterns <- manyTill digit (lexeme (char '|'))
  output <- manyTill digit eol
  pure (patterns, output)

parser :: Parser [SignalPattern]
parser = manyTill signalPattern eof

part1 :: Parsed [SignalPattern] -> IO ()
part1 input = do
  let answer = length
             . filter ((`elem` [2,3,4,7]))
             . concat
             . map (map length . snd)
           <$> input
  printAnswer "Times 1, 4, 7 or 8 appear: " answer

part2 :: Parsed [SignalPattern] -> IO ()
part2 input = do
  let answer = const "P"  <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 08: Seven Segment Search"
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
