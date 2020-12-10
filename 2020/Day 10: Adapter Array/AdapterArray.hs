module Main where

import Criterion.Main
import qualified Data.List as L
import Data.Void (Void)
import System.FilePath ((</>))
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Paths_AoC2020

type Parser = Parsec Void String
type Parsed a = Either (ParseErrorBundle String Void) a

data Distribution = Distribution { one :: Integer
                                 , two :: Integer
                                 , three :: Integer
                                 }
  deriving Show

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space hspace1 empty empty)

integer :: Parser Integer
integer = lexeme L.decimal

adapters :: Parser [Integer]
adapters = sepEndBy integer eol

readInput :: String -> IO (Parsed [Integer])
readInput day = do
  inputFile <- getDataFileName (day </> "input.txt")
  parse adapters inputFile
    <$> readFile inputFile

printAnswer :: Show a => String -> Parsed a -> IO ()
printAnswer question answer =
  either (putStrLn . errorBundlePretty)
         (putStrLn . (question <>) . show)
         answer

joltDistribution :: Distribution -> Integer -> [Integer] -> Distribution
joltDistribution distrib@Distribution{three=th} _ [] =
  distrib {three = th + 1}
joltDistribution distrib@Distribution{one=o, two=tw, three=th} last (x:xs)
  | x - last == 1 = joltDistribution (distrib {one = o + 1}) x xs
  | x - last == 2 = joltDistribution (distrib {two = tw + 1}) x xs
  | x - last == 3 = joltDistribution (distrib {three = th + 1}) x xs

part1 :: Parsed [Integer] -> IO ()
part1 input = do
  let answer = (\d -> one d * three d)
             . joltDistribution (Distribution 0 0 0) 0
             . L.sort
           <$> input
  printAnswer "Product of 1- and 3-jolt differences: " answer

oneSequences :: [Integer] -> [Integer]
oneSequences adapters = foldr tally end adapters 0 0
  where
    end _ running | running > 1 = [running - 1]
                  | otherwise = []
    tally adap next last running | adap - last == 1 = next adap (running + 1)
                                 | running > 1 = (running - 1) : next adap 0
                                 | otherwise = next adap 0

combinations :: Integer -> Integer
combinations 1 = 2
combinations 2 = 4
combinations 3 = 7
combinations 4 = 13

part2 :: Parsed [Integer] -> IO ()
part2 input = do
  let answer = L.foldl' (*) 1
             . map combinations
             . oneSequences
             . L.sort
           <$> input
  printAnswer "Distinct adapter combinations: " answer

main :: IO ()
main = do
  let day =  "Day 10: Adapter Array"
  input <- readInput day
  putStrLn ""
  part1 input
  part2 input
  putStrLn ""
  defaultMain [
      bgroup "AoC"
        [ bench "Part 1" $ nfIO (silence $ readInput day >>= part1)
        , bench "Part 2" $ nfIO (silence $ readInput day >>= part2)
        ]
    ]
