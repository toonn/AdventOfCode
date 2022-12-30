module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.ExtendedReal as E
import qualified Data.Sequence as Seq
import qualified Data.IntegerInterval as II

import AoC

import Debug.Trace

type Input = [Int]

parser :: Parser Input
parser = sepEndBy (signed integer) eol <* eof

coordinates :: Seq.Seq Int -> [Int]
coordinates mixed = map (Seq.index mixed . (`rem` cypherLength)) [1000, 2000, 3000]
  where
    cypherLength = Seq.length mixed

mix :: Input -> Seq.Seq Int
mix cypher
  = (\(mixed,shifts) ->
      let (upto0,from0) = Seq.breakl (== 0) mixed
       in from0 <> upto0
    )
  . Seq.foldlWithIndex
      (\(mixed,shifts) index shift ->
        let index' = foldl (\i (s, shifted) ->
                             let i' | II.member (fromIntegral i) shifted = i + s
                                    | otherwise = i
                              in i'
                           )
                           index
                           shifts
            (front, _ Seq.:<| back) = Seq.splitAt index' mixed
            newIndex = let i = (index' + shift) `rem` (cypherLength - 1)
                           i' | i > 0 = i
                              | otherwise = cypherLength - 1 + i
                        in i'
            (direction,shifted)
              | index' < newIndex = (-1, interval index' newIndex)
              | index' > newIndex = (1, interval newIndex index')
              | otherwise = (0, II.empty)
            shifts' | direction == 0 = shifts
                    | otherwise = shifts Seq.|> (direction,shifted)
            mixed' | direction == -1
                   = front <> Seq.insertAt (newIndex - index') shift back
                   | direction == 1 = Seq.insertAt newIndex shift front <> back
                   | direction == 0 = front <> (shift Seq.<| back)
         in (mixed', shifts')
      )
      (cypherSeq, Seq.empty)
  $ cypherSeq
  where
    cypherSeq = Seq.fromList cypher
    cypherLength = Seq.length cypherSeq
    interval a b = (E.Finite . fromIntegral $ a)
         II.<=..<= (E.Finite . fromIntegral $ b)

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . coordinates . mix <$> input
  printAnswer "Sum of the coordinates: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const "P" <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 20: Grove Positioning System"
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
