module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AoC

data RPS = R | P | S deriving Eq

type Pairing = (Char, Char)
type Score = Int

pairing :: Parser Pairing
pairing = do
  l <- oneOf "ABC"
  hspace
  r <- oneOf "XYZ"
  pure (l, r)

parser :: Parser [Pairing]
parser = manyTill (pairing <* optional eol) eof

rps :: Char -> RPS
rps 'A' = R
rps 'B' = P
rps 'C' = S
rps 'X' = R
rps 'Y' = P
rps 'Z' = S

shapeScore :: RPS -> Score
shapeScore R = 1
shapeScore P = 2
shapeScore S = 3

outcomeScore :: RPS -> RPS -> Score
outcomeScore R S = 0
outcomeScore P R = 0
outcomeScore S P = 0
outcomeScore l r | l == r = 3
outcomeScore R P = 6
outcomeScore P S = 6
outcomeScore S R = 6

score :: Pairing -> Score
score (l, r) = shapeScore r' + outcomeScore l' r'
  where
    l' = rps l
    r' = rps r

accordingGuide :: [Pairing] -> Score
accordingGuide = sum . map score

part1 :: Parsed [Pairing] -> IO ()
part1 input = do
  let answer = accordingGuide <$> input
  printAnswer "Total score according to guide: " answer

shapeValue :: Score -> Char -> Score
shapeValue opponent 'X' = case opponent - 1 of
                            0 -> 3
                            r -> r
shapeValue opponent 'Y' = opponent
shapeValue opponent 'Z' = case opponent + 1 of
                            4 -> 1
                            r -> r

outcomeValue :: Char -> Score
outcomeValue 'X' = 0
outcomeValue 'Y' = 3
outcomeValue 'Z' = 6

correctScore :: Pairing -> Score
correctScore (opponent, outcome) =
  shapeValue (shapeScore . rps $ opponent) outcome + outcomeValue outcome

correctInterpretation :: [Pairing] -> Score
correctInterpretation = sum . map correctScore

part2 :: Parsed [Pairing] -> IO ()
part2 input = do
  let answer = correctInterpretation <$> input
  printAnswer "Total score according to correct interpretation: " answer

main :: IO ()
main = do
  let day = "Day 02: Rock Paper Scissors"
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
