module Main where

import Criterion.Main
import qualified Data.Map as M
import Data.Void (Void)
import System.FilePath ((</>))
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Paths_AoC2020

type Parser = Parsec Void String
type Parsed a = Either (ParseErrorBundle String Void) a

type Numbers = (Integer, Integer, M.Map Integer Integer)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space hspace1 empty empty)

integer :: Parser Integer
integer = lexeme L.decimal

numbers :: Parser Numbers
numbers = do
  ns <- sepBy integer (char ',') <* eol <* eof
  let seen = foldr (\(i,n) m -> M.insert n i m) M.empty (zip [1..] (init ns))
  pure (fromIntegral (length ns), last ns, seen)

readInput :: String -> IO (Parsed Numbers)
readInput day = do
  inputFile <- getDataFileName (day </> "input.txt")
  parse numbers inputFile
    <$> readFile inputFile

printAnswer :: Show a => String -> Parsed a -> IO ()
printAnswer question answer =
  either (putStrLn . errorBundlePretty)
         (putStrLn . (question <>) . show)
         answer

th :: Integer -> [Numbers] -> Integer
th i ((j, n, _):ns) | i == j = n
                    | i < j = error "Can't index starting numbers"
                    | otherwise = th i ns

age :: Numbers -> Integer
age (i, n, seen) = case seen M.!? n of
  Nothing -> 0
  Just j -> i - j

recite :: Numbers -> Numbers
recite (i, n, seen) = (i+1, age (i, n, seen), M.insert n i seen)

part1 :: Parsed Numbers -> IO ()
part1 input = do
  let answer = (2020 `th`) . iterate recite <$> input
  printAnswer "The 2020th number is: " answer

part2 :: Parsed Numbers -> IO ()
part2 input = do
  let answer = const 'P'
           <$> input
  printAnswer "Not an answer: " answer

main :: IO ()
main = do
  let day = "Day 15: Rambunctious Recitation"
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
