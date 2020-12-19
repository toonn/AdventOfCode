module Main where

import Criterion.Main
import Control.Monad (guard)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Void (Void)
import System.FilePath ((</>))
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Paths_AoC2020

type Parser = Parsec Void String
type Parsed a = Either (ParseErrorBundle String Void) a

data Rule = Char Char
          | Match [Integer]
          | Choice [Rule]
  deriving Show
type Rules = M.Map Integer Rule
type Messages = [String]

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space hspace1 empty empty)

integer :: Parser Integer
integer = lexeme L.decimal

charRule :: Parser Rule
charRule = lexeme ( do
  char '"'
  c <- char 'a' <|> char 'b'
  char '"'
  pure (Char c)
                )

matchRule :: Parser Rule
matchRule = Match <$> some integer

accumulate :: [Rule] -> Rule
accumulate [m] = m
accumulate ms = Choice ms

choiceRule :: Parser Rule
choiceRule = accumulate <$> sepBy matchRule (lexeme (char '|'))

rule :: Parser (Integer,Rule)
rule = do
  index <- integer
  char ':'
  hspace
  body <- charRule <|> choiceRule
  pure (index, body)

rules :: Parser Rules
rules = M.fromList <$> sepEndBy rule eol

messages :: Parser Messages
messages = sepEndBy (takeWhile1P (Just "Character") (`elem` "ab")) eol

lists :: Parser (Rules, Messages)
lists = do
  r <- rules
  eol
  m <- messages
  eof
  pure (r, m)

readInput :: String -> IO (Parsed (Rules, Messages))
readInput day = do
  inputFile <- getDataFileName (day </> "input.txt")
  parse lists inputFile
    <$> readFile inputFile

printAnswer :: Show a => String -> Parsed a -> IO ()
printAnswer question answer =
  either (putStrLn . errorBundlePretty)
         (putStrLn . (question <>) . show)
         answer

pass :: Rules -> [Integer] -> String -> Bool
pass _ [] [] = True
pass _ [] _ = False
pass _ _ [] = False
pass rules (i:is) s@(x:xs) = case rules M.! i of
  Char c | c == x -> pass rules is xs
         | otherwise -> False
  Match js -> pass rules (js <> is) s
  Choice rs -> foldr (\(Match js) next -> pass rules (js <> is) s || next)
                     False
                     rs

validMessages :: (Rules, Messages) -> Messages
validMessages (rules, messages) = filter (pass rules [0]) messages

part1 :: Parsed (Rules, Messages) -> IO ()
part1 input = do
  let answer = length . validMessages <$> input
  printAnswer "Nr of messages matching rule 0: " answer

updateRules8And11 :: (Rules, Messages) -> (Rules, Messages)
updateRules8And11 (rs, ms) = (update rs, ms)
  where
    update = M.insert 8 (Choice [Match [42],Match [42,8]])
           . M.insert 11 (Choice [Match [42,31],Match [42,11,31]])

part2 :: Parsed (Rules, Messages) -> IO ()
part2 input = do
  let answer = length . validMessages . updateRules8And11 <$> input
  printAnswer "Nr of matching messages after updating rules 8 and 11: " answer

main :: IO ()
main = do
  let day = "Day 19: Monster Messages"
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
