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

type Rule = (String, [(String, Integer)])
type Rules a = M.Map String [a]

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
  pure $ (container, containees)

rules :: Parser [Rule]
rules = sepEndBy rule eol

readInput :: IO (Parsed [Rule])
readInput = do
  inputFile <- getDataFileName "Day 7: Handy Haversacks/input.txt"
  parse rules inputFile
    <$> readFile inputFile

printAnswer :: Show a => String -> Parsed a -> IO ()
printAnswer question answer =
  either (putStrLn . errorBundlePretty)
         (putStrLn . (question <>) . show)
         answer

closure :: String -> Rules String -> S.Set String
closure k m = case M.lookup k m of
  Nothing -> S.empty
  Just [] -> error ("A " <> k <> " bag cannot be contained in any other bag.")
  Just bs -> S.unions (map (\k' -> S.insert k' (closure k' m)) bs)

part1 :: Parsed [Rule] -> IO ()
part1 input = do
  -- Data.Map.Merge.Strict may have more efficient combinators than union after
  -- mapping.
  let answer = S.size . closure "shiny gold" . M.unionsWith (<>)
             . map (\(k, bs) -> foldr (\(b,_) -> M.insert b [k]) M.empty bs)
             <$> input
  printAnswer "Nr of bags that can contain a shiny gold bag: " answer

closure' :: String -> Rules (String, Integer) -> Integer
closure' k m = case M.lookup k m of
  Nothing -> error ("I don't know of any " <> k <> " bags.")
  Just [] -> 0
  Just bs -> foldr (\(b,qty) total -> total + qty * (1 + closure' b m))
                   0
                   bs

part2 :: Parsed [Rule] -> IO ()
part2 input = do
  let answer = closure' "shiny gold" . M.fromList <$> input
  printAnswer "Nr of bags a shiny gold bag needs to contain: " answer

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
