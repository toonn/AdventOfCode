module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Set as S

import AoC

type Coord = (Int, Int)
type Rock = S.Set Coord
type Sand = S.Set Coord
type Blocked = S.Set Coord

type Input = Rock

coord :: Parser Coord
coord = do
  x <- integer
  char ','
  y <- integer
  pure (x, y)

rock :: Parser Rock
rock = S.unions
     . map S.fromAscList
     . (\l -> zipWith (\(x1,y1) (x2,y2) ->
                        case (x1 `compare` x2, y1 `compare` y2) of
                          (LT,EQ) -> zip [x1..x2] (repeat y1)
                          (GT,EQ) -> zip [x2..x1] (repeat y1)
                          (EQ,LT) -> zip (repeat x1) [y1..y2]
                          (EQ,GT) -> zip (repeat x1) [y2..y1]
                      )
                      l
                      (tail l)
       )
   <$> sepBy coord (lexeme (string "->"))

parser :: Parser Input
parser = S.unions <$> manyTill (rock <* eol) eof

sandSource :: Coord
sandSource = (500, 0)

dropUnit :: Rock -> Sand -> Int -> Coord -> Maybe Sand
dropUnit _ _ abyssDepth (_,y) | y > abyssDepth = Nothing
dropUnit rocks sandAtRest abyssDepth (x,y)
  | let down = (x,y+1), S.notMember down blocked
  = dropUnit rocks sandAtRest abyssDepth down
  | let downLeft = (x-1,y+1), S.notMember downLeft blocked
  = dropUnit rocks sandAtRest abyssDepth downLeft
  | let downRight = (x+1,y+1), S.notMember downRight blocked
  = dropUnit rocks sandAtRest abyssDepth downRight
  where blocked = rocks `S.union` sandAtRest
dropUnit _ sandAtRest _ unit = Just (S.insert unit sandAtRest)

simulateSand :: Rock -> Sand
simulateSand rocks = foldr (\unit more sandAtRest ->
                             case dropUnit rocks sandAtRest abyssDepth unit of
                               Nothing -> sandAtRest
                               Just sandAtRest' -> more sandAtRest'
                           )
                           id
                           (repeat sandSource)
                           S.empty
  where
    abyssDepth = S.findMax . S.map snd $ rocks

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = S.size . simulateSand <$> input
  printAnswer "Units at rest before sand flows into the abyss: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const "P" <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 14: Regolith Reservoir"
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
