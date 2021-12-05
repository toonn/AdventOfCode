module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Set as S

import AoC

type Point = (Int, Int)
type Line = (Point, Point)

coord :: Parser Point
coord = do
  x <- integer
  char ','
  y <- integer
  pure (x, y)

line :: Parser Line
line = do
  start <- coord
  lexeme (string "->")
  end <- coord
  eol
  pure (start, end)

parser :: Parser [Line]
parser = manyTill line eof

horizontalOrVertical :: Line -> Bool
horizontalOrVertical ((x1,y1), (x2,y2)) | x1 == x2 = True
                                        | y1 == y2 = True
                                        | otherwise = False

linePoints :: Line -> S.Set Point
linePoints ((x1,y1), (x2,y2))
  | x1 == x2 = S.fromAscList (map (\y -> (x1,y)) [y'..y''])
  | y1 == y2 = S.fromAscList (map (\x -> (x,y1)) [x'..x''])
  | otherwise = S.fromAscList (zip [x'..x''] (range y' y''))
  where
    (x',y',x'',y'') | x1 < x2 = (x1,y1,x2,y2)
                    | x2 < x1 = (x2,y2,x1,y1)
                    | y1 < y2 = (x1,y1,x2,y2)
                    | otherwise = (x2,y2,x1,y1)

    range a b | a < b = [a..b]
              | otherwise = reverse [b..a]

intersections :: [S.Set Point] -> S.Set Point
intersections = fst . foldr (\points (ints, alls) ->
                              ( S.union (S.intersection points alls) ints
                              , S.union points alls)
                            )
                            (S.empty, S.empty)

part1 :: Parsed [Line] -> IO ()
part1 input = do
  let answer = S.size . intersections . map linePoints . filter horizontalOrVertical <$> input
  printAnswer "Overlaps: " answer

part2 :: Parsed [Line] -> IO ()
part2 input = do
  let answer = S.size . intersections . map linePoints <$> input
  printAnswer "Overlaps including diagonals: " answer

main :: IO ()
main = do
  let day = "Day 05: Hydrothermal Venture"
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
