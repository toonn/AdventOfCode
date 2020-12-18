module Main where

import Criterion.Main
import qualified Data.Monoid as M
import Data.Void (Void)
import System.FilePath ((</>))
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Paths_AoC2020

type Parser = Parsec Void String
type Parsed a = Either (ParseErrorBundle String Void) a

data Expression = Value Integer
                | Sum Expression Expression
                | Product Expression Expression
  deriving Show
type Expressions = [Expression]

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space hspace1 empty empty)

integer :: Parser Integer
integer = lexeme L.decimal

value :: Parser Expression
value = (Value <$> integer)
    <|> lexeme (between (char '(') (char ')') expression)

rhs :: Expression -> Parser Expression
rhs l = do
  o <- char '+' <|> char '*'
  hspace
  r <- value
  let l' = case o of
             '+' -> Sum l r
             '*' -> Product l r
  rhs l' <|> pure l'

expression :: Parser Expression
expression = do
  l <- value
  (rhs l  <|> pure l)

expressions :: Parser Expressions
expressions = sepEndBy expression eol <* eof

readInput :: String -> IO (Parsed Expressions)
readInput day = do
  inputFile <- getDataFileName (day </> "input.txt")
  parse expressions inputFile
    <$> readFile inputFile

printAnswer :: Show a => String -> Parsed a -> IO ()
printAnswer question answer =
  either (putStrLn . errorBundlePretty)
         (putStrLn . (question <>) . show)
         answer

evaluate :: Expression -> Integer
evaluate (Value v) = v
evaluate (Sum l r) = evaluate l + evaluate r
evaluate (Product l r) = evaluate l * evaluate r

part1 :: Parsed Expressions -> IO ()
part1 input = do
  let answer = M.getSum . mconcat . map M.Sum . map evaluate <$> input
  printAnswer "Sum of results: " answer

part2 :: Parsed Expressions -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "Not an answer: " answer

main :: IO ()
main = do
  let day = "Day 18: Operation Order"
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
