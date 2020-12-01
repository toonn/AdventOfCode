module Main where

import Control.Monad.State.Strict
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Paths_AoC2020

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space (newline *> return ()) empty empty)

integer :: Parser Integer
integer = lexeme L.decimal

parseEntries :: Parser [Integer]
parseEntries = manyTill integer eof

readEntries :: String -> IO (Either (ParseErrorBundle String Void) [Integer])
readEntries inputFile = parse parseEntries inputFile <$> readFile inputFile

checkSums :: Integer -> [Integer] -> Maybe Integer
checkSums entry entries = foldr (\e e' -> case e' of
                                  Nothing | entry + e == 2020 -> Just e
                                  _ -> e'
                                )
                                Nothing
                                entries

multiply2020Sum :: [Integer] -> Integer
multiply2020Sum [] = 0
multiply2020Sum (entry:entries) = go 0
  where
    go multiplication =
      case checkSums entry entries of
        Nothing -> multiply2020Sum entries
        Just entry' -> entry * entry'

multiplyTriple :: [Integer] -> Integer
multiplyTriple [] = 0
multiplyTriple (entry:entries) = case go entries of
  0 -> multiplyTriple entries
  x -> x
  where
    go [] = 0
    go (e:es) = case checkSums (entry + e) es of
      Nothing -> go es
      Just e' -> entry * e * e'

main :: IO ()
main = do
  inputFile <- getDataFileName "Day 1: Report Repair/input.txt"
  entries <- readEntries inputFile
  let expenseMultiplication = multiply2020Sum <$> entries
  either (putStrLn . errorBundlePretty)
         (putStrLn . ("Multiplication of two expenses: " <>) . show)
         expenseMultiplication
  let expenseThree = multiplyTriple <$> entries
  either (putStrLn . errorBundlePretty)
         (putStrLn . ("Multiplication of three expenses: " <>) . show)
         expenseThree
