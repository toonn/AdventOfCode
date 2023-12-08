module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Map as M

import AoC

type Instructions = String
type Node = String

type Input = (Instructions, [(Node, (Node, Node))])

instructions :: Parser Instructions
instructions = takeWhile1P (Just "instruction") (`elem` "RL")

node :: Parser Node
node = takeP (Just "Label character") 3

edges :: Parser (Node, (Node, Node))
edges = do
  f <- lexeme node
  lexeme (char '=')
  es <- between (char '(')
                (char ')')
                ((,) <$> node <* (lexeme (char ',')) <*> node)
  pure (f, es)

parser :: Parser Input
parser = do
  is <- instructions
  eol
  eol
  es <- sepEndBy edges eol
  eof
  pure (is, es)

stepsToZZZ :: Input -> Int
stepsToZZZ (is, es) = let network = M.fromList es
                       in snd
                        . foldr (\i more (current, steps) ->
                                  let options = network M.! current
                                      next | i == 'L'  = fst options
                                           | otherwise = snd options
                                      steps' | current == "ZZZ"
                                             = (current, steps)
                                             | otherwise
                                             = more (next, steps + 1)
                                   in steps'
                                )
                                id
                                (cycle is)
                        $ ("AAA", 0)

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = stepsToZZZ <$> input
  printAnswer "Steps to reach ZZZ: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 08: Haunted Wasteland"
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
