module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import qualified Data.Set as S

type Input = [(S.Set Int,[S.Set Int],[Int])]

betwixt :: Char -> Char -> Parser a -> Parser a
betwixt open close = between (char open) (char close)

commaSeparatedNumbers :: Parser [Int]
commaSeparatedNumbers = sepEndBy1 integer (char ',')

diagram :: Parser (S.Set Int)
diagram = S.fromAscList
        . map fst
        . filter ((== '#') . snd)
        . zip [0..]
      <$> betwixt '[' ']' (some (anySingleBut ']'))

schematics :: Parser [S.Set Int]
schematics = sepEndBy1 ( S.fromAscList
                     <$> betwixt '(' ')' commaSeparatedNumbers
                       ) hspace1

joltageRequirements :: Parser [Int]
joltageRequirements = betwixt '{' '}' commaSeparatedNumbers

parser :: Parser Input
parser = sepEndBy1 ( (,,)
                 <$> diagram
                  <* hspace1
                 <*> schematics
                 <*> joltageRequirements
                   )
                   eol
      <* eof

-- symmetricDifference requires containers >= 0.8 :'(
symmetricDifference :: S.Set Int -> S.Set Int -> S.Set Int
symmetricDifference s t = (s S.\\ t) <> (t S.\\ s)

fewestPresses :: (S.Set Int, [S.Set Int], [Int]) -> Int
fewestPresses (target, buttons, _)
  = aStar neighbors distance heuristic isGoal start
  where
    neighbors configuration = map (symmetricDifference configuration) buttons
    distance = const (const 1)
    heuristic = const 0
    isGoal = (== target)
    start = mempty

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . map fewestPresses <$> input
  printAnswer "Fewest presses: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 10: Factory"
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
