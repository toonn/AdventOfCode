module Main where

import Data.Bits (xor)
import Data.Char (isLetter)
import Data.Maybe (listToMaybe)
import qualified Data.Vector as V
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Paths_AoC2020

type Parser = Parsec Void String
type Parsed a = Either (ParseErrorBundle String Void) a

type Policy = (Integer, Integer, Char)
type Password = V.Vector Char

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
password = lexeme (takeWhile1P (Just "Password character") (isLetter)) >>= pure . V.fromList

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
  let count = toInteger $ V.length (V.filter (== le) pas)
  in lo <= count && count <= up

countValidPasswords :: ((Policy, Password) -> Bool)
                    -> [(Policy, Password)]
                    -> Integer
countValidPasswords p = toInteger . length . filter p

validPasswordMkII :: (Policy, Password) -> Bool
validPasswordMkII ((lo, up, le), pas) =
  case (pas V.!? (fromInteger lo - 1), pas V.!? (fromInteger up - 1)) of
    (Just x, Just y) | (x == le) `xor` (y == le) -> True
    (Just x, Nothing) -> x == le
    (Nothing, Just y) -> y == le
    _ -> False

main :: IO ()
main = do
  inputFile <- getDataFileName "Day 2: Password Philosophy/input.txt"
  database <- readDB inputFile
  let validNr = countValidPasswords validPassword <$> database
  either (putStrLn . errorBundlePretty)
         (putStrLn . ("Nr of valid passwords: " <>) . show)
         validNr
  let validNrMkII = countValidPasswords validPasswordMkII <$> database
  either (putStrLn . errorBundlePretty)
         (putStrLn . ("Nr of valid passwords according corporate policy: " <>)
         . show)
         validNrMkII


