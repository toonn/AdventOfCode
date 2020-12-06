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

anyone :: Parser GroupAnswer
anyone = mconcat <$> endBy answers eol

groupAnswers :: Parser GroupAnswer -> Parser [GroupAnswer]
groupAnswers groupAnswer = sepEndBy groupAnswer eol

readInput :: Parser GroupAnswer -> IO (Parsed [GroupAnswer])
readInput groupAnswer = do
  inputFile <- getDataFileName "Day 6: Custom Customs/input.txt"
  parse (groupAnswers groupAnswer) inputFile
    <$> readFile inputFile

printAnswer :: Show a => String -> Parsed a -> IO ()
printAnswer question answer =
  either (putStrLn . errorBundlePretty)
         (putStrLn . (question <>) . show)
         answer

part1 :: (Parser GroupAnswer -> IO (Parsed [GroupAnswer])) -> IO ()
part1 input = do
  input' <- input anyone
  let sumOfGroupCounts = sum . map S.size <$> input'
  printAnswer "Anyone answered yes: " sumOfGroupCounts

everyone :: Parser GroupAnswer
everyone = foldr1 S.intersection <$> endBy answers eol

part2 :: (Parser GroupAnswer -> IO (Parsed [GroupAnswer])) -> IO ()
part2 input = do
  input' <- input everyone
  let answer = sum . map S.size <$> input'
  printAnswer "Everyone answered yes: " answer

main :: IO ()
main = do
  putStrLn ""
  part1 readInput
  part2 readInput
  putStrLn ""
  defaultMain [
      bgroup "AoC"
        [ bench "Part 1" $ nfIO (silence $ part1 readInput)
        , bench "Part 2" $ nfIO (silence $ part2 readInput)
        ]
    ]
