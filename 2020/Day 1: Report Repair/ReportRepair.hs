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
multiply2020Sum (entry:entries) = go 0
  where
    go multiplication =
      case checkSums entry entries of
        Nothing -> multiply2020Sum entries
        Just entry' -> entry * entry'

main :: IO ()
main = do
  inputFile <- getDataFileName "Day 1: Report Repair/input.txt"
  entries <- readEntries inputFile
  let expenseMultiplication = multiply2020Sum <$> entries
  either (putStrLn . errorBundlePretty)
         (putStrLn . ("Multiplication of two expenses: " <>) . show)
         expenseMultiplication
