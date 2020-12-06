module Main where

import Criterion.Main
import Data.Char (isLower)
import qualified Data.Set as S
import Data.Void (Void)
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Paths_AoC2020

type Parser = Parsec Void String
type Parsed a = Either (ParseErrorBundle String Void) a

type Answer = S.Set Char
type GroupAnswer = Answer

answers :: Parser Answer
answers = S.fromList <$> takeWhile1P (Just "Answer") isLower

groupAnswer :: Parser GroupAnswer
groupAnswer = mconcat <$> endBy answers eol

groupAnswers :: Parser [GroupAnswer]
groupAnswers = sepEndBy groupAnswer eol

readInput :: IO (Parsed [GroupAnswer])
readInput = do
  inputFile <- getDataFileName "Day 6: Custom Customs/input.txt"
  parse groupAnswers inputFile
    <$> readFile inputFile

printAnswer :: Show a => String -> Parsed a -> IO ()
printAnswer question answer =
  either (putStrLn . errorBundlePretty)
         (putStrLn . (question <>) . show)
         answer

part1 :: Parsed [GroupAnswer] -> IO ()
part1 input = do
  let sumOfGroupCounts = sum . map S.size <$> input
  printAnswer "Sum of counts: " sumOfGroupCounts

part2 :: Parsed [GroupAnswer] -> IO ()
part2 input = do
  printAnswer "" (pure "Not an answer")

main :: IO ()
main = do
  input <- readInput
  part1 input
  part2 input
  defaultMain [
      bgroup "AoC"
        [ bench "Part 1" $ nfIO (silence $ readInput >>= part1)
        , bench "Part 2" $ nfIO (silence $ readInput >>= part2)
        ]
    ]
