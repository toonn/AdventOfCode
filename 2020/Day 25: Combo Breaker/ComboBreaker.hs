{-# LANGUAGE RankNTypes #-}
module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AoC

type Key = Int

publicKeys :: Parser (Int, Int)
publicKeys = do
  k1 <- integer
  eol
  k2 <- integer
  eol
  eof
  pure (k1, k2)

step :: Int -> Int -> Int
step subjectNr v = (subjectNr * v) `rem` 20201227

transform ::  Int -> Int -> Int
transform l subjectNr = go l 1
  where
    go 0 v = v `rem` 20201227
    go l v = go (l-1) (step subjectNr v)

loopSize :: (Key, Key) -> (Int, Key)
loopSize (k1,k2) = foldr ( \v next l ->
                            let lK | v == k1 = (l, k2)
                                   | v == k2 = (l, k1)
                                   | otherwise = next (l + 1)
                            in lK
                         )
                         (error "No suitable loop size smaller than infinity")
                         (iterate (step 7) 7)
                         1

secret :: (Key, Key) -> Key
secret keys = uncurry transform (loopSize keys)

part1 :: Parsed (Key, Key) -> IO ()
part1 input = do
  let answer = secret <$> input
  printAnswer "Encryption key: " answer

part2 :: Parsed (Key, Key) -> IO ()
part2 input = do
  let answer = const "No part 2 : )" <$> input
  printAnswer "Psych: " answer

main :: IO ()
main = do
  let day = "Day 25: Combo Breaker"
  let parser = publicKeys
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
