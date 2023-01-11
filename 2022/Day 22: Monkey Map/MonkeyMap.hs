module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Set as S

import AoC

import Debug.Trace

type Coord = Int
type Coords = (Coord, Coord)
type Board = (S.Set Coords, S.Set Coords)

data Instruction = Move Int | Turn Char deriving Show

type Path = [Instruction]

type Direction = Int
type Position = (Coord, Coord, Direction)

type Input = (Board, Path)

row :: Parser ([Coord], [Coord])
row = foldr (\(x,c) (opens,walls) ->
              let ts | c == '.' = (x:opens, walls)
                     | c == '#' = (opens, x:walls)
                     | otherwise = (opens, walls)
               in ts
            )
            ([],[])
    . zip [1..]
  <$> takeWhile1P (Just "Tile") (`elem` " .#")

board :: Parser Board
board = foldr (\(y,(os,ws)) (opens,walls) ->
                ( S.union opens (S.fromAscList (map (\x -> (y,x)) os))
                , S.union walls (S.fromAscList (map (\x -> (y,x)) ws))
                )
              )
              (S.empty, S.empty)
      . zip [1..]
    <$> sepEndBy row eol

instruction :: Parser Instruction
instruction = Move <$> integer <|> Turn <$> satisfy (`elem` "LR")

path :: Parser Path
path = manyTill instruction eol

parser :: Parser Input
parser = do b <- board
            eol
            p <- path
            eof
            pure (b, p)

north, east, south, west :: Int
(north, east, south, west) = (3, 0, 1, 2)

turn :: Direction -> Char -> Direction
turn orientation 'L' = (orientation - 1 + 4) `rem` 4
turn orientation 'R' = (orientation + 1) `rem` 4

wrap :: Board -> Direction -> Coords -> Either Coords Coords
wrap (open,wall) o (y,x) = eNext
  where
    tiles = S.union open wall
    -- Even directions 0 and 2, east and west, are horizontal.
    slice | even o = S.filter (\(y',_) -> y == y') tiles
          | otherwise = S.filter (\(_,x') -> x == x') tiles
    -- Directions 0 and 1, east and south, are positive changes to the
    -- coordinate.
    next | o < 2 = S.findMin slice
         | otherwise = S.findMax slice
    eNext | next `S.member` wall = Left next
          | next `S.member` open = Right next
          | otherwise = error (show next)

move :: Board -> Direction -> Coords -> Int -> Position
move _ o (y,x) 0 = (y, x, o)
move (open,wall) o (y,x) d = next
  where
    step | o == north = (y - 1, x)
         | o == east = (y, x + 1)
         | o == south = (y + 1, x)
         | o == west = (y, x - 1)
    next | step `S.member` open = move (open, wall) o step (d - 1)
         | step `S.member` wall = (y, x, o)
         | otherwise = case wrap (open, wall) o (y, x) of
                         Left _ -> (y, x, o)
                         Right step' -> move (open, wall) o step' (d - 1)

follow :: Board -> Path -> Position
follow (open,wall) path = go path start
  where
    start = (\(y,x) -> (y,x,east)) (S.findMin open)

    go [] pos = pos
    go (Turn d:is) (y,x,o) = go is (y, x, turn o d)
    go (Move d:is) (y,x,o) = go is (move (open, wall) o (y,x) d)

password :: Position -> Int
password (y,x,o) = 1000 * y + 4 * x + o

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = password . uncurry follow <$> input
  printAnswer "Final password: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const "P" <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 22: Monkey Map"
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
