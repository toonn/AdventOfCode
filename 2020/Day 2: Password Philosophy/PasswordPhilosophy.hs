module Main where

import Data.Char (isLetter)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Paths_AoC2020

type Parser = Parsec Void String
type Parsed a = Either (ParseErrorBundle String Void) a

type Policy = (Integer, Integer, Char)
type Password = String

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space space1 empty empty)

integer :: Parser Integer
integer = lexeme L.decimal

policy :: Parser Policy
policy = do
  lower <- integer
  char '-'
  upper <- integer
  letter <- letterChar
  pure (lower, upper, letter)

password :: Parser Password
password = lexeme (takeWhile1P (Just "Password character") (isLetter))

entry :: Parser (Policy, Password)
entry = do
  pol <- policy
  lexeme (char ':')
  pas <- password
  pure (pol, pas)

parseDB :: Parser [(Policy, Password)]
parseDB = manyTill entry eof

readDB :: String -> IO (Parsed [(Policy, Password)])
readDB inputFile = parse parseDB inputFile <$> readFile inputFile

validPassword :: (Policy, Password) -> Bool
validPassword ((lo, up, le), pas) =
  let count = toInteger $ length (filter (== le) pas)
  in lo <= count && count <= up

countValidPasswords :: [(Policy, Password)] -> Integer
countValidPasswords = toInteger . length . filter validPassword

main :: IO ()
main = do
  inputFile <- getDataFileName "Day 2: Password Philosophy/input.txt"
  database <- readDB inputFile
  let validNr = countValidPasswords <$> database
  either (putStrLn . errorBundlePretty)
         (putStrLn . ("Nr of valid passwords: " <>) . show)
         validNr

