module Main where

import Criterion.Main
import Data.Char (isLower)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Void (Void)
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Paths_AoC2020

type Parser = Parsec Void String
type Parsed a = Either (ParseErrorBundle String Void) a

type Rule = M.Map String (String, Integer)
type Rules = M.Map String [(String, Integer)]

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space hspace1 empty empty)

comma :: Parser Char
comma = lexeme (char ',')

integer :: Parser Integer
integer = lexeme L.decimal

word :: String -> Parser String
word s = lexeme (string s)

bag :: Parser String
bag = do
  modifier <- lexeme (takeWhile1P (Just "Modifier") isLower)
  color <- lexeme (takeWhile1P (Just "Color") isLower)
  string "bag"
  (lexeme (char 's') *> pure ()) <|> hspace
  pure $ modifier <> " " <> color

bags :: Parser (String, Integer)
bags = do
  quantity <- integer
  kind <- bag
  pure (kind, quantity)

rule :: Parser Rule
rule = do
  container <- bag
  word "contain"
  containees <- (string "no other bags" *> pure []) <|> sepBy bags comma
  char '.'
  pure $ foldr (\(ctee, qty) m -> M.insert ctee (container, qty) m)
               M.empty
               containees

rules :: Parser Rules
rules = M.unionsWith (<>) . map (M.map (:[])) <$> sepEndBy rule eol

readInput :: IO (Parsed Rules)
readInput = do
  inputFile <- getDataFileName "Day 7: Handy Haversacks/input.txt"
  parse rules inputFile
    <$> readFile inputFile

printAnswer :: Show a => String -> Parsed a -> IO ()
printAnswer question answer =
  either (putStrLn . errorBundlePretty)
         (putStrLn . (question <>) . show)
         answer

closure :: String -> Rules -> S.Set String
closure k m = case M.lookup k m of
  Nothing -> S.empty
  Just [] -> error ("A " <> k <> " bag cannot be contained in any other bag.")
  Just bs -> S.unions (map (\(k', _) -> S.insert k' (closure k' m)) bs)

part1 :: Parsed Rules -> IO ()
part1 input = do
  let answer = S.size . closure "shiny gold" <$> input
  printAnswer "Nr of bags that can contain a shiny gold bag: " answer

part2 :: Parsed Rules -> IO ()
part2 input = do
  let answer = const (-1) <$> input
  printAnswer "Not an answer: " answer

main :: IO ()
main = do
  input <- readInput
  putStrLn ""
  part1 input
  part2 input
  putStrLn ""
  defaultMain [
      bgroup "AoC"
        [ bench "Part 1" $ nfIO (silence $ readInput >>= part1)
        , bench "Part 2" $ nfIO (silence $ readInput >>= part2)
        ]
    ]
