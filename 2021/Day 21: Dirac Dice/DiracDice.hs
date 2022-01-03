module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec hiding (chunk)
import Text.Megaparsec.Char

import AoC

type Player = (Int, Int)
type Players = (Player, Player)
type Die = [Int]

parser :: Parser Players
parser = do
  string "Player 1 starting position: "
  p1 <- integer
  eol
  string "Player 2 starting position: "
  p2 <- integer
  eol
  eof
  return ((p1,0),(p2,0))

position :: Int -> Int -> Int
position start roll = case (start + roll) `mod` 10 of
                        0 -> 10
                        n -> n

turn :: Die -> Players -> (Die, Players)
turn die ((p1,s1), (p2,s2)) = (die'', ((p1', s1'), player2))
  where
    (p1Rolls, die') = splitAt 3 die
    (p2Rolls, die'') = splitAt 3 die'
    p1' = position p1 (sum p1Rolls)
    s1' = s1 + p1'
    player2 | s1' >= 1000 = (p2,s2)
            | p2' <- position p2 (sum p2Rolls) = (p2', s2 + p2')

play :: Die -> Players -> Int
play die players = foldr (\(r, (_, ((_,s1),(_,s2)))) n ->
                           let result | s1 >= 1000 = s2 * (r * 6 - 3)
                                      | s2 >= 1000 = s1 * (r * 6)
                                      | otherwise  = n
                            in result
                         )
                         (error "Found the end of an infinite list!")
                         (zip [0..] rounds)
  where
    rounds = iterate (uncurry turn) (die, players)

deterministicDie :: Die
deterministicDie = [1..100] <> deterministicDie

part1 :: Parsed Players -> IO ()
part1 input = do
  let answer = play deterministicDie <$> input
  printAnswer "Losing score times number of die rolls: " answer

part2 :: Parsed Players -> IO ()
part2 input = do
  let answer = const "P" <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 21: Dirac Dice"
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
