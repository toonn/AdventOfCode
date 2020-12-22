module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AoC

import qualified Data.Set as S
import qualified Data.Sequence as Seq

type Deck = Seq.Seq Integer
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
  pure (Seq.fromList deck1, Seq.fromList deck2)

play :: Decks -> Decks
play (card1 Seq.:<| cards1, card2 Seq.:<| cards2)
  | card1 > card2 = play ( cards1 Seq.|> card1 Seq.|> card2
                         , cards2
                         )
  | otherwise     = play ( cards1
                         , cards2 Seq.|> card2 Seq.|> card1
                         )
play decks = decks

cardCount :: Integer -> Deck -> Integer
cardCount _ Seq.Empty = 0
cardCount factor (cards Seq.:|> card) =
  factor * card + cardCount (factor + 1) cards

score :: Decks -> Integer
score (deck1, deck2) = let cards | Seq.null deck1 = deck2
                                 | otherwise = deck1
                        in cardCount 1 cards

part1 :: Parsed Decks -> IO ()
part1 input = do
  let answer = score . play <$> input
  printAnswer "Winning player's score: " answer

playRecursive :: S.Set Decks -> Decks -> Decks
playRecursive seen decks | S.member decks seen = decks
playRecursive seen decks@(card1 Seq.:<| cards1, card2 Seq.:<| cards2)
  = playRecursive (S.insert decks seen) decks'
  where
    player1Victory
      | card1 <= fromIntegral (Seq.length cards1)
        && card2 <= fromIntegral (Seq.length cards2)
        = case playRecursive S.empty
                             ( Seq.take (fromIntegral card1) cards1
                             , Seq.take (fromIntegral card2) cards2
                             ) of
            (deck1, _) | Seq.null deck1 -> False
                       | otherwise      -> True
      | card1 > card2 = True
      | otherwise     = False
    decks' | player1Victory = ( cards1 Seq.|> card1 Seq.|> card2
                              , cards2
                              )
           | otherwise      = ( cards1
                              , cards2 Seq.|> card2 Seq.|> card1
                              )
playRecursive _ decks = decks

part2 :: Parsed Decks -> IO ()
part2 input = do
  let answer = score . playRecursive S.empty <$> input
  printAnswer "Winning score of the recursive game: " answer

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
