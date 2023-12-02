module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.IntMap as IM
import Data.Char (digitToInt, isAlphaNum, isDigit)

import AoC

type ID = Int
type Draw = Int
type Set = (Draw, Draw, Draw)
type Draws = [Draw]
type Game = (Draws, Draws, Draws)

type Input = IM.IntMap Game

draw :: Parser Set
draw = do
  n <- integer
  inT <- lexeme ( (string "red" *> pure (\a -> (a,0,0)))
                <|> (string "green" *> pure (\b -> (0,b,0)))
                <|> (string "blue" *> pure (\c -> (0,0,c)))
                )
  pure $ inT n

(+.) :: Num a => (a, a, a) -> (a, a, a) -> (a, a, a)
(a,b,c) +. (a',b',c') = (a + a', b + b', c + c')

set :: Parser Set
set = foldr (+.) (0,0,0) <$> sepBy draw (lexeme (char ','))

game :: Parser (ID, Game)
game = do
  lexeme (string "Game")
  iD <- lexeme integer
  lexeme (char ':')
  g <- unzip3 <$> sepBy set (lexeme (char ';'))
  pure (iD, g)

parser :: Parser Input
parser = IM.fromAscList <$> sepEndBy game eol <* eof

mostSeen :: Game -> Set
mostSeen (rs, gs, bs) = (maximum rs, maximum gs, maximum bs)

possible :: Set -> Bool
possible (r,g,b) = r <= 12 && g <= 13 && b <= 14

filterPossible :: Input -> IM.IntMap Set
filterPossible = IM.filter possible . IM.map mostSeen

sumPossibleIDs :: Input -> Int
sumPossibleIDs = sum . IM.keys . filterPossible

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sumPossibleIDs <$> input
  printAnswer "Sum of possible game IDs: " answer

power :: Set -> Int
power (r,g,b) = r * g * b

sumPowers :: Input -> Int
sumPowers = sum . IM.elems . IM.map (power . mostSeen)

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = sumPowers <$> input
  printAnswer "Sum of powers: " answer

main :: IO ()
main = do
  let day = "Day 02: Cube Conundrum"
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
