module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AoC

import Data.Char (digitToInt, intToDigit, isDigit)
import Data.Foldable (toList)
import qualified Data.Sequence as Seq

type Cups = Seq.Seq Integer

cups :: Parser Cups
cups = Seq.fromList . map (fromIntegral . digitToInt)
   <$> (takeWhile1P (Just "Digit") isDigit <* eol <* eof)

destination :: Integer -> Cups -> Cups -> Integer
destination 0 three cups = destination (maximum (three <> cups)) three cups
destination cup three cups = case Seq.elemIndexL cup three of
                               Nothing -> cup
                               Just _ -> destination (cup - 1) three cups

play :: Integer -> Cups -> Cups
play 0 cups = cups
play moves (current Seq.:<| cups) =
  let (three, cups') = Seq.splitAt 3 cups
      dest = destination (current - 1) three cups'
      (before, after)
        | Just index <- Seq.elemIndexL dest cups'
          = Seq.splitAt (index + 1) cups'
   in play (moves - 1) (before <> three <> after Seq.:|> current)

labels :: Cups -> String
labels cups | Just index <- Seq.elemIndexL 1 cups =
  case Seq.splitAt index cups of
    (before, _ Seq.:<| after) ->
      map (intToDigit . fromIntegral) (toList (after <> before))

part1 :: Parsed Cups -> IO ()
part1 input = do
  let answer = labels . play 100 <$> input
  printAnswer "Labels after cup 1: " answer

part2 :: Parsed Cups -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "Not an answer: " answer

main :: IO ()
main = do
  let day = "Day 23: Crab Cups"
  let parser = cups
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
