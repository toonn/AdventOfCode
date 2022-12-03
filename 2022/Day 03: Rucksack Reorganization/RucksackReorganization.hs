module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Char (isLetter)
import Data.List (elemIndex, splitAt)
import Data.Maybe (fromJust)
import qualified Data.Set as S

import AoC

type Item = Char
type Rucksack = (S.Set Item, S.Set Item)
type Priority = Int

rucksack :: Parser Rucksack
rucksack = do
  items <- takeWhileP (Just "item") isLetter
  let (l,r) = splitAt (length items `quot` 2) items
  pure (S.fromList l, S.fromList r)

parser :: Parser [Rucksack]
parser = manyTill (rucksack <* eol) eof

inBoth :: Rucksack -> S.Set Item
inBoth = uncurry S.intersection

priority :: Item -> Priority
priority item = (+1) . fromJust $ elemIndex item (['a'..'z'] <> ['A'..'Z'])

typePrioritySum :: [Rucksack] -> Priority
typePrioritySum = sum . map (S.foldr ((+) . priority) 0 . inBoth)

part1 :: Parsed [Rucksack] -> IO ()
part1 input = do
  let answer = typePrioritySum <$> input
  printAnswer "Sum of priorities of types: " answer

part2 :: Parsed [Rucksack] -> IO ()
part2 input = do
  let answer = const "P" <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 03: Rucksack reorganization"
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
