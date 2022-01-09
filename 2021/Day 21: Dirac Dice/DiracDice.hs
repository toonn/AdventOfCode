module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec hiding (chunk)
import Text.Megaparsec.Char

import Data.List (groupBy, partition, sortBy)

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

quantumDice :: [(Int, Int)]
quantumDice = zip [1,3,6,7,6,3,1] [3..9]

possibleRolls :: Int
possibleRolls = sum . map fst $ quantumDice

quantumTurn :: Int -> Player -> Player
quantumTurn roll (p,s) = (p', s + p')
  where
    p' = position p roll

futures :: [Player -> (Int, Player)]
futures = (\(n,r) p -> (n, quantumTurn r p)) <$> quantumDice

allFutures :: Player -> [(Int,Int)]
allFutures (p,s) | s >= 21 = [(1,0)]
                 | otherwise = map (\tss -> ( sum . map fst $ tss
                                            , snd . head $ tss
                                            )
                                   )
                             . groupBy (\(_,s1) (_,s2) -> s1 == s2)
                             . sortBy (\(_,s1) (_,s2) -> s1 `compare` s2)
                             . concatMap (\(n,p') ->
                                           map (\(times,steps) ->
                                                 (n * times, steps + 1)
                                               )
                                               (allFutures p')
                                         )
                             . map ($ (p,s))
                             $ futures

playDirac :: Players -> Int
playDirac (p1,p2) = max p1Wins p2Wins
  where
    p1Fs = allFutures p1
    p2Fs = allFutures p2

    p1Wins = sum
           . map (\(times, steps) ->
                   let losingFutures2 = possibleRolls ^ (steps - 1)
                                      - ( sum
                                        . map (\(ts,stps) ->
                                                ts * 27 ^ (steps - 1 - stps)
                                              )
                                        . filter ((< steps) . snd)
                                        $ p2Fs
                                        )
                    in times * losingFutures2
                 )
           $ p1Fs

    p2Wins = sum
           . map (\(times, steps) ->
                   let losingFutures1 = possibleRolls ^ steps
                                      - ( sum
                                        . map (\(ts,stps) ->
                                                ts * 27 ^ (steps - stps)
                                              )
                                        . filter ((<= steps) . snd)
                                        $ p1Fs
                                        )
                    in times * losingFutures1
                 )
           $ p2Fs

part2 :: Parsed Players -> IO ()
part2 input = do
  let answer = playDirac <$> input
  printAnswer "Universes the winning player wins in: " answer

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
