module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Map as M
import qualified Data.Set as S

import AoC

type ID = Int
type Nr = Int
type Card = (S.Set Nr, S.Set Nr)

type Input = M.Map ID Card

card :: Parser (ID, Card)
card = do
  lexeme (string "Card")
  iD <- integer
  lexeme (char ':')
  winningNrs <- S.fromList <$> many integer
  lexeme (char '|')
  havingNrs <- S.fromList <$> many integer
  pure (iD, (winningNrs, havingNrs))

parser :: Parser Input
parser = M.fromAscList <$> sepEndBy card eol <* eof

winningNrs :: Card -> S.Set Nr
winningNrs = uncurry S.intersection

points :: S.Set Nr -> Maybe Int
points wNrs | count <- S.size wNrs, count > 0 = Just (2 ^ (count - 1))
            | otherwise = Nothing

totalPoints :: Input -> Int
totalPoints = sum . M.elems . M.mapMaybe (points . winningNrs)

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = totalPoints <$> input
  printAnswer "Total points: " answer

-- totalCards :: Input -> Int
totalCards original = let scores = M.map (S.size . winningNrs) original
                          initialCards = M.map (const 1) original
                       in sum
                        . M.elems
                        . M.foldrWithKey (\iD score more nrCards ->
                                              let n = M.findWithDefault 0
                                                                        iD
                                                                        nrCards
                                                  copyIDs = [ iD + s
                                                            | s <- [1..score]
                                                            ]
                                               in more (foldr (M.adjust (+n))
                                                              nrCards
                                                              copyIDs
                                                       )
                                         )
                                         id
                                         scores
                        $ initialCards

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = totalCards <$> input
  printAnswer "Total scratchcards: " answer

main :: IO ()
main = do
  let day = "Day 04: Scratchcards"
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
