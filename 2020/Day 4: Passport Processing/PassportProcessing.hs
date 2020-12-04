module Main where

import Data.Char (isAlphaNum, isDigit, isHexDigit, isLetter)
import qualified Data.Map as M
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Paths_AoC2020

type Parser = Parsec Void String
type Parsed a = Either (ParseErrorBundle String Void) a

type Passport = M.Map String String
type Batch = [Passport]

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space hspace1 empty empty)

fieldChar :: String -> (Char -> Bool)
fieldChar key = M.findWithDefault (const False) key $ M.fromDistinctAscList
    [ ("byr", isDigit)
    , ("cid", isDigit)
    , ("ecl", \a -> a == '#' || isAlphaNum a)
    , ("eyr", isDigit)
    , ("hcl", \a -> a == '#' || isAlphaNum a)
    , ("hgt", isAlphaNum)
    , ("iyr", isDigit)
    , ("pid", \a -> a == '#' || isAlphaNum a)
    ]

field :: Parser (String, String)
field = do
  key <- takeWhile1P (Just "Key character") isLetter
  char ':'
  value <- takeWhile1P (Just "Value character") (fieldChar key)
  pure (key, value)

passport :: Parser Passport
passport = M.fromList <$> sepEndBy field ((eol *> pure ()) <|> hspace1)

parseBatchFile :: Parser Batch
parseBatchFile = sepBy passport eol <* eof

readBatchFile :: String -> IO (Parsed Batch)
readBatchFile inputFile = parse parseBatchFile inputFile
  <$> readFile inputFile

countPassports :: Batch -> Int
countPassports = length . filter (\p -> M.size p == 8 || M.size p == 7 && M.notMember "cid" p)

main :: IO ()
main = do
  inputFile <- getDataFileName "Day 4: Passport Processing/input.txt"
  batchFile <- readBatchFile inputFile
  let validNr = countPassports  <$> batchFile
  either (putStrLn . errorBundlePretty)
         (putStrLn . ("Nr of valid passports: " <>) . show)
         validNr
