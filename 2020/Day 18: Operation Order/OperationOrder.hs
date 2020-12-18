module Main where

import Criterion.Main
import Control.Monad.Combinators.Expr
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
type Table = [[Operator Parser Expression]]

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space hspace1 empty empty)

integer :: Parser Integer
integer = lexeme L.decimal

value :: Parser Expression
value = (Value <$> integer)

term :: Table -> Parser Expression
term table = lexeme (between (char '(') (char ')') (expression table)) <|> value

expression :: Table -> Parser Expression
expression table = makeExprParser (term table) table

expressions :: Table -> Parser Expressions
expressions table = sepEndBy (expression table) eol <* eof

readInput :: String -> Table -> IO (Parsed Expressions)
readInput day table = do
  inputFile <- getDataFileName (day </> "input.txt")
  parse (expressions table) inputFile
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

sumResults :: Expressions -> Integer
sumResults = M.getSum . mconcat . map M.Sum . map evaluate

basicOperators :: Table
basicOperators = [ [ InfixL (Sum <$ lexeme (char '+'))
                   , InfixL (Product <$ lexeme (char '*'))
                   ]
                 ]

part1 :: (Table -> IO (Parsed Expressions)) -> IO ()
part1 input = do
  input' <- input basicOperators
  let answer = sumResults <$> input'
  printAnswer "Sum of results: " answer

advancedOperators :: Table
advancedOperators = [ [ InfixL (Sum <$ lexeme (char '+')) ]
                 , [ InfixL (Product <$ lexeme (char '*')) ]
                 ]

part2 :: (Table -> IO (Parsed Expressions)) -> IO ()
part2 input = do
  input' <- input advancedOperators
  let answer = sumResults <$> input'
  printAnswer "Sum of advanced results: " answer

main :: IO ()
main = do
  let day = "Day 18: Operation Order"
  let input = readInput day
  putStrLn ""
  part1 input
  part2 input
  putStrLn ""
  defaultMain [
      bgroup "AoC"
        [ bench "Part 1" $ nfIO (silence $ part1 $ readInput day)
        , bench "Part 2" $ nfIO (silence $ part2 $ readInput day)
        ]
    ]
