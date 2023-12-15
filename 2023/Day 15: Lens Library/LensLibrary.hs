module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

type Input = [String]

parser :: Parser Input
parser = sepBy1 (takeWhile1P (Just "Non comma or newline character")
                             (not . (`elem` ",\n"))
                )
                (char ',')
      <* eol <* eof

hASH :: Enum a => [a] -> Int
hASH string = foldr (\c more currentValue ->
                      more
                    . (`rem` 256)
                    . (17 *)
                    . (currentValue +)
                    . fromEnum
                    $ c
                    )
                    id
                    string
                    0

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . map hASH <$> input
  printAnswer "Total load on north support beams: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 15: Lens Library"
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
