module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Char (digitToInt)
import qualified Data.IntMap as IM
import Data.List (transpose, zipWith4)
import qualified Data.Set as S
import Data.Tuple (swap)

import AoC

type Direction = Char
type Distance = Int
type Instruction = (Direction, Distance)
type Coord = (Int, Int)

type Input = [Instruction]

instruction :: Parser Instruction
instruction = do
  dir <- oneOf "UDLR"
  hspace
  dist <- integer
  pure (dir, dist)

parser :: Parser Input
parser = manyTill (instruction <* eol) eof

move :: Direction -> Coord -> Coord
move dir (hX,hY) | dir == 'U' = (hX, hY + 1)
                 | dir == 'D' = (hX, hY - 1)
                 | dir == 'L' = (hX - 1, hY)
                 | dir == 'R' = (hX + 1, hY)

distance :: Coord -> Coord -> Int
distance (x,y) (x',y') = max (abs (x - x')) (abs (y - y'))

pull :: Instruction -> Coord -> Coord -> (Coord, Coord, S.Set Coord)
pull (dir, dist) hP tP = go dist hP tP (S.singleton tP)
  where
    go 0 hP tP vs = (hP, tP, vs)
    go d hP tP vs = let d' = d - 1
                        hP' = move dir hP
                        (tP', vs') | distance hP' tP <= 1 = (tP, vs)
                                   | otherwise = (hP, S.insert hP vs)
                     in go d' hP' tP' vs'

tailVisited :: Input -> S.Set Coord
tailVisited instructions =
  let start = (0,0)
   in foldr (\inst visited headPosition tailPosition ->
              case pull inst headPosition tailPosition of
                (hP, tP, vs) -> vs `S.union` (visited hP tP)
            )
            (const S.singleton)
            instructions
            start
            start

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = S.size . tailVisited <$> input
  printAnswer "Number of positions visited by the tail: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const "P" <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 09: Rope Bridge"
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
