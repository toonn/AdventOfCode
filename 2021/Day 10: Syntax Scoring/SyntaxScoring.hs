module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec hiding (chunk)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.List (sort)
import Data.Maybe (mapMaybe)

import AoC

type Chunk = String

chunk :: Parser Chunk
chunk = manyTill (satisfy (`elem` "()[]{}<>")) eol

parser :: Parser [Chunk]
parser = manyTill chunk eof

corrupt :: Chunk -> ([Char], Maybe Char)
corrupt = go []
  where
    go [] [] = ([], Nothing)
    go (e:es) [] = (e:es , Nothing)
    go expect (c:cs) | c == '(' = go (')':expect) cs
                     | c == '[' = go (']':expect) cs
                     | c == '{' = go ('}':expect) cs
                     | c == '<' = go ('>':expect) cs
                     | c `elem` ")]}>" = case expect of
                                           (e:es) | c == e -> go es cs
                                                  | otherwise -> ([], Just c)
                                           es -> (es, Nothing)

errorScore :: Char -> Int
errorScore ')' = 3
errorScore ']' = 57
errorScore '}' = 1197
errorScore '>' = 25137

part1 :: Parsed [Chunk] -> IO ()
part1 input = do
  let answer = sum . map errorScore . mapMaybe (snd . corrupt) <$> input
  printAnswer "Corrupted chunk syntax score: " answer

completionScore :: [Char] -> Int
completionScore = go 0
  where
    points :: Char -> Int
    points ')' = 1
    points ']' = 2
    points '}' = 3
    points '>' = 4

    go s [] = s
    go s (c:cs) = go (5 * s + points c) cs

middle :: [Int] -> Int
middle xs = head (drop (length xs `quot` 2) (sort xs))

part2 :: Parsed [Chunk] -> IO ()
part2 input = do
  let answer = middle
             . map completionScore
             . mapMaybe ((\(es, _) -> case es of
                           [] -> Nothing
                           es -> Just es
                         ) . corrupt
                        )
           <$> input
  printAnswer "Middle score of completions: " answer

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
