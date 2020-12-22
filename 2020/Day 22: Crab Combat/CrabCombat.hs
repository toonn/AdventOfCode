module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AoC

import qualified Data.Sequence as S

type Deck = S.Seq Integer
type Decks = (Deck, Deck)

decks :: Parser Decks
decks = do
  string "Player 1:"
  eol
  deck1 <- endBy integer eol
  eol
  string "Player 2:"
  eol
  deck2 <- endBy integer eol
  eof
  pure (S.fromList deck1, S.fromList deck2)

play :: Decks -> Decks
play (card1 S.:<| cards1, card2 S.:<| cards2)
  | card1 > card2 = play ( cards1 S.|> card1 S.|> card2
                         , cards2
                         )
  | otherwise     = play ( cards1
                         , cards2 S.|> card2 S.|> card1
                         )
play decks = decks

cardCount :: Integer -> Deck -> Integer
cardCount _ S.Empty = 0
cardCount factor (cards S.:|> card) =
  factor * card + cardCount (factor + 1) cards

score :: Decks -> Integer
score (deck1, deck2) = cardCount 1 deck1 + cardCount 1 deck2

part1 :: Parsed Decks -> IO ()
part1 input = do
  let answer = score . play <$> input
  printAnswer "Winning player's score: " answer

part2 :: Parsed Decks -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "Not an answer: " answer

main :: IO ()
main = do
  let day = "Day 22: Crab Combat"
  let parser = decks
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
