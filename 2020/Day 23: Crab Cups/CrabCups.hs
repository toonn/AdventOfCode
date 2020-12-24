{-# LANGUAGE RankNTypes #-}
module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AoC

import Control.Monad.ST
import Data.Char (digitToInt, intToDigit, isDigit)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import qualified Data.Vector.Generic.Mutable as GM

type Cups = U.Vector Int
type MCups s = MU.MVector s Int

cups :: Parser [Int]
cups = map digitToInt <$> (takeWhile1P (Just "Digit") isDigit <* eol <* eof)

destination :: Int -> Int -> Int -> Int -> Int -> Int
destination max a b c cup | cup == 0 = destination max a b c max
                          | a /= cup && b /= cup && c /= cup = cup
                          | otherwise = destination max a b c (cup - 1)

play :: forall s . Int -> Int -> MCups s -> ST s (MCups s)
play 0 _ cups = pure cups
play moves current cups = do
  a <- MU.read cups current
  b <- MU.read cups a
  c <- MU.read cups b
  next <- MU.read cups c
  let d = destination (MU.length cups - 1) a b c (current - 1)
  next' <- MU.read cups d
  MU.write cups current next
  MU.write cups d a
  MU.write cups c next'
  play (moves - 1) next cups

labels :: Cups -> String
labels cups = go (cups U.! 1)
  where
    go 1 = []
    go i = intToDigit i : go (cups U.! i)

part1 :: Parsed [Int] -> IO ()
part1 input = do
  let answer = labels
             . (\(current, cups) ->
                 runST (U.thaw cups >>= play 100 current >>= U.freeze)
               )
             . (\cups -> ( head cups
                         , (U.replicate (length cups + 1) 0)
                           U.// (zip cups (tail cups <> [head cups]))
                         )
               )
           <$> input
  printAnswer "Labels after cup 1: " answer

productTwo :: Cups -> Int
productTwo cups = x * y
  where
    x = cups U.! 1
    y = cups U.! x

part2 :: Parsed [Int] -> IO ()
part2 input = do
  let answer = productTwo
             . (\(current, cups) ->
                 runST (U.thaw cups >>= play 10000000 current >>= U.freeze)
               )
             . (\cups -> ( head cups
                         , (U.enumFromN 1 (1000000 + 1))
                           U.// (zip (1000000:cups) (cups <> [length cups + 1]))
                         )
               )
           <$> input
  printAnswer "Product of two cups following label 1: " answer

main :: IO ()
main = do
  let day = "Day 23: Crab Cups"
  let parser = cups
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
