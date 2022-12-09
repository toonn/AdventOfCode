module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.List (intercalate)
import qualified Data.Set as S

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

follow :: Coord -> [Coord] -> S.Set Coord -> ([Coord], S.Set Coord)
follow prevP [] vs = ([prevP], S.insert prevP vs)
follow prevP@(prevX,prevY) (p@(pX,pY):ps) vs
  | distance prevP p <= 1 = (prevP:p:ps, vs)
  | let p' | prevX == pX && prevY == pY + 2 = (pX, pY + 1)
           | (prevX == pX + 1 && prevY == pY + 2)
             || (prevX == pX + 2 && prevY == pY + 2)
             || (prevX == pX + 2 && prevY == pY + 1) = (pX + 1, pY + 1)
           | prevX == pX + 2 && prevY == pY = (pX + 1, pY)
           | (prevX == pX + 2 && prevY == pY - 1)
             || (prevX == pX + 2 && prevY == pY - 2)
             || (prevX == pX + 1 && prevY == pY - 2) = (pX + 1, pY - 1)
           | prevX == pX && prevY == pY - 2 = (pX, pY - 1)
           | (prevX == pX - 1 && prevY == pY - 2)
             || (prevX == pX - 2 && prevY == pY - 2)
             || (prevX == pX - 2 && prevY == pY - 1) = (pX - 1, pY - 1)
           | prevX == pX - 2 && prevY == pY = (pX - 1, pY)
           | (prevX == pX - 2 && prevY == pY + 1)
             || (prevX == pX - 2 && prevY == pY + 2)
             || (prevX == pX - 1 && prevY == pY + 2) = (pX - 1, pY + 1)
           | otherwise = error (intercalate "\n" [show prevP , show p , show ps, show vs])
        (ps', vs') = follow p' ps vs
    = (prevP:ps', vs')

pull :: Instruction -> [Coord] -> ([Coord], S.Set Coord)
pull (dir, dist) positions = go dist positions (S.singleton (last positions))
  where
    go 0 ps vs = (ps, vs)
    go d (hP:ps) vs = let d' = d - 1
                          hP' = move dir hP
                          (ps', vs') = follow hP' ps vs
                       in go d' ps' vs'

tailVisited :: Int -> Input -> S.Set Coord
tailVisited knots instructions =
  let start = (0,0)
   in foldr (\inst visited positions ->
              case pull inst positions of
                (ps, vs) -> vs `S.union` (visited ps)
            )
            (S.singleton . last)
            instructions
            (replicate knots start)

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = S.size . tailVisited 2 <$> input
  printAnswer "Number of positions visited by the tail: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = S.size . tailVisited 10 <$> input
  printAnswer "Number of positions visited by the long tail: " answer

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
