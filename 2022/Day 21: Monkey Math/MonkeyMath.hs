module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Map as M

import AoC

type Monkey = String

data Expression = Lit Int
                | Op Char Monkey Monkey
                deriving Show

type Input = M.Map Monkey Expression

name :: Parser String
name = takeWhile1P (Just "Monkey name") (`elem` ['a'..'z'])

operator :: Parser (Monkey -> Monkey -> Expression)
operator = Op <$> choice (map char "+-*/")

monkey :: Parser (Monkey, Expression)
monkey = do
  m <- name
  char ':'
  hspace
  expr <- (do l <- lexeme name
              op <- lexeme operator
              r <- lexeme name
              return (l `op` r)
          )
          <|> Lit <$> integer
  pure (m, expr)

parser :: Parser Input
parser = M.fromList <$> sepEndBy monkey eol <* eof

op :: Char -> Int -> Int -> Int
op '+' = (+)
op '-' = (-)
op '*' = (*)
op '/' = quot

calculate :: Monkey -> Input -> Int
calculate monkey = fst . go monkey
  where
    go m ms = let e = ms M.! m
                  (result,ms') | Lit l <- e = (l, ms)
                               | Op o m1 m2 <- e
                               , (l,lMs) <- go m1 ms
                               , (r,rMs) <- go m2 lMs
                               , let result = op o l r
                               = (result, M.insert m (Lit result) rMs)
               in (result, ms')

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = calculate "root" <$> input
  printAnswer "Root yells: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const "P" <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 21: Monkey Math"
  input <- readInput day parser
  putStrLn ""
  part1 input
  part2 input
  putStrLn ""
  defaultMain [
      bgroup "AoC"
        [ bench "Part 1" $ nfIO (silence $ readInput day parser >>= part1)
        , bench "Part 2" $ nfIO (silence $ readInput day parser >>= part2)
        ]
    ]
