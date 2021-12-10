module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec hiding (chunk)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Maybe (mapMaybe)

import AoC

type Chunk = String

chunk :: Parser Chunk
chunk = manyTill (satisfy (`elem` "()[]{}<>")) eol

parser :: Parser [Chunk]
parser = manyTill chunk eof

score :: Char -> Int
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137

corrupt :: Chunk -> Maybe Char
corrupt = go []
  where
    go [] [] = Nothing
    go (e:_) [] = Nothing
    go expect (c:cs) | c == '(' = go (')':expect) cs
                     | c == '[' = go (']':expect) cs
                     | c == '{' = go ('}':expect) cs
                     | c == '<' = go ('>':expect) cs
                     | c `elem` ")]}>" = case expect of
                                           (e:es) | c == e -> go es cs
                                                  | otherwise -> Just c
                                           _ -> Nothing

part1 :: Parsed [Chunk] -> IO ()
part1 input = do
  let answer = sum . map score . mapMaybe corrupt <$> input
  printAnswer "Corrupted chunk syntax score: " answer

part2 :: Parsed [Chunk] -> IO ()
part2 input = do
  let answer = const "P" <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 10: Syntax Scoring"
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
