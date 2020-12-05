module Main where

import Control.Applicative (Alternative)
import Control.Monad (guard)
import Data.Char (isAlphaNum, isDigit, isLetter, isSpace)
import qualified Data.Map as M
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Paths_AoC2020

type Parser = Parsec Void String
type Parsed a = Either (ParseErrorBundle String Void) a

type Passport a = M.Map String a
type Batch a = [Passport a]

data Field = Byr Integer | Iyr Integer | Eyr Integer | Hgt Integer String
           | Hcl String | Ecl String | Pid String | Cid String
  deriving (Eq, Show)

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

field1 :: Parser (String, String)
field1 = do
  key <- takeWhile1P (Just "Key character") isLetter
  char ':'
  value <- takeWhile1P (Just "Value character") (fieldChar key)
  pure (key, value)

consumeValue :: Parser (Maybe a)
consumeValue = takeWhile1P (Just "Non-whitespace character") (not . isSpace)
  *> pure Nothing

integer :: Parser Integer
integer = L.decimal

withinBounds :: Alternative f => Integer -> Integer -> Integer -> f ()
withinBounds lo x hi = guard (lo <= x && x <= hi)

year :: Integer -> Integer -> Parser Integer
year lo up = do
  year <- integer
  withinBounds lo year up
  pure year

byr :: Parser (Maybe Field)
byr = try (Just . Byr <$> year 1920 2002) <|> consumeValue

iyr :: Parser (Maybe Field)
iyr = try (Just . Iyr <$> year 2010 2020) <|> consumeValue

eyr :: Parser (Maybe Field)
eyr = try (Just . Eyr <$> year 2020 2030) <|> consumeValue

hgt :: Parser (Maybe Field)
hgt = try (do
  h <- integer
  u <- string "cm" <|> string "in"
  case u of
    "cm" -> withinBounds 150 h 193
    "in" -> withinBounds 59 h 76
  pure (Just (Hgt h u)))
  <|> consumeValue

hcl :: Parser (Maybe Field)
hcl = try (do
   char '#'
   Just . Hcl <$> count 6 (satisfy (`elem` "0123456789abcdef"))
  )
  <|> consumeValue

ecl :: Parser (Maybe Field)
ecl = choice (map (fmap (Just . Ecl) . string)
                  ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
             )
  <|> consumeValue

pid :: Parser (Maybe Field)
pid = try (do
    id <- takeWhile1P (Just "Pid digit") isDigit
    guard (length id == 9)
    pure . Just . Pid $ id
  ) <|> consumeValue

cid :: Parser (Maybe Field)
cid = consumeValue


field2 :: Parser (String, Maybe Field)
field2 = do
  key <- takeWhile1P (Just "Key character") isLetter
  char ':'
  value <- (case key of
              "byr" -> byr
              "iyr" -> iyr
              "eyr" -> eyr
              "hgt" -> hgt
              "hcl" -> hcl
              "ecl" -> ecl
              "pid" -> pid
              "cid" -> cid
           )
  pure (key, value)

passport :: Parser (String, a) -> Parser (Passport a)
passport field = M.fromList . filter (\(k,v) -> k /= "cid") <$> sepEndBy field ((eol *> pure ()) <|> hspace1)

parseBatchFile :: Parser (String, a) -> Parser (Batch a)
parseBatchFile field = sepBy (passport field) eol <* eof

readBatchFile :: Parser (String, a) -> String -> IO (Parsed (Batch a))
readBatchFile field inputFile = parse (parseBatchFile field) inputFile
  <$> readFile inputFile

countPassports :: (Batch a) -> Int
countPassports = length . filter (\p -> M.size p == 7)

main :: IO ()
main = do
  inputFile <- getDataFileName "Day 4: Passport Processing/input.txt"
  batchFile <- readBatchFile field1 inputFile
  let validNr = countPassports <$> batchFile
  either (putStrLn . errorBundlePretty)
         (putStrLn . ("Nr of valid passports: " <>) . show)
         validNr
  batchFile2 <- readBatchFile field2 inputFile
  let validRequiredFieldsNr = countPassports . map (M.filter (/= Nothing))
                           <$> batchFile2
  either (putStrLn . errorBundlePretty)
         (putStrLn . ("Nr of passports with required fields: " <>) . show)
         validRequiredFieldsNr
