module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec hiding (chunk)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Set as S

import AoC

type Dot = (Int, Int)
type Page = (Int, Int, S.Set Dot)
type Instruction = (Char, Int)

dot :: Parser Dot
dot = do
  x <- integer
  char ','
  y <- integer
  eol
  pure (x,y)

instruction :: Parser Instruction
instruction = do
  string "fold along "
  direction <- char 'x' <|> char 'y'
  char '='
  place <- integer
  eol
  pure (direction, place)

parser :: Parser (Page, [Instruction])
parser = do
  dots <- manyTill dot eol
  instructions <- manyTill instruction eof
  let (xM, yM) = foldr (\(x,y) (mx,my) ->
                         case (compare x mx, compare y my) of
                           (GT, GT) -> (x,y)
                           (GT, _ ) -> (x,my)
                           (_, GT ) -> (mx,y)
                           _        -> (mx,my)
                       )
                       (0,0)
                       dots
  pure ((xM, yM, S.fromList dots), instructions)

fold :: Instruction -> Page -> Page
fold ('x', c) (xM, yM, dots) =
  ( xM'
  , yM
  , S.foldr (\(x,y) ds -> case compare x c of
      LT -> S.insert (x + dLT, y) ds
      EQ -> ds
      GT -> S.insert (dGT - x, y) ds
    )
    S.empty
    dots
  )
  where
    overlap = 2*c - xM
    (xM', dLT, dGT) | overlap < 0 = (xM - c, -overlap, xM          )
                    | otherwise   = (c - 1 , 0       , xM + overlap)
fold ('y', r) (xM, yM, dots) =
  ( xM
  , yM'
  , S.foldr (\(x,y) ds -> case compare y r of
      LT -> S.insert (x, y + dLT) ds
      EQ -> ds
      GT -> S.insert (x, dGT - y) ds
    )
    S.empty
    dots
  )
  where
    overlap = 2*r - yM
    (yM', dLT, dGT) | overlap < 0 = (xM - r, -overlap, yM          )
                    | otherwise   = (r - 1 , 0       , yM + overlap)

visibleDots :: Page -> Int
visibleDots (_, _, dots) = S.size dots

part1 :: Parsed (Page, [Instruction]) -> IO ()
part1 input = do
  let answer = visibleDots . (\(page, is) -> fold (head is) page)  <$> input
  printAnswer "Visible dots after first fold: " answer

part2 :: Parsed (Page, [Instruction]) -> IO ()
part2 input = do
  let answer = const "P" <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 13: Transparent Origami"
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
