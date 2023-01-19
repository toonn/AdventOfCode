module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.List ((!!), find)
import Data.Maybe (fromJust)
import qualified Data.Set as S

import AoC
import CubeNets

type Board = (S.Set Coords, S.Set Coords)

data Instruction = Move Int | Turn Char deriving Show

type Path = [Instruction]

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
                ( S.union opens (S.fromDistinctAscList (map (\x -> (y,x)) os))
                , S.union walls (S.fromDistinctAscList (map (\x -> (y,x)) ws))
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

wrapFlat :: Board -> Position -> Either Position Position
wrapFlat (open,wall) (y,x,o) = eith (y', x', o)
  where
    tiles = S.union open wall
    -- Even directions 0 and 2, east and west, are horizontal.
    slice | even o = S.filter (\(y',_) -> y == y') tiles
          | otherwise = S.filter (\(_,x') -> x == x') tiles
    -- Directions 0 and 1, east and south, are positive changes to the
    -- coordinate.
    (y',x') | o < 2 = S.findMin slice
          | otherwise = S.findMax slice
    eith | (y',x') `S.member` wall = Left
         | (y',x') `S.member` open = Right

move :: (Board -> Position -> Either Position Position)
     -> Board -> Position -> Int -> Position
move _ _ pos 0 = pos
move wrap (open,wall) (y,x,o) d = next
  where
    (y',x') | o == north = (y - 1, x)
            | o == east = (y, x + 1)
            | o == south = (y + 1, x)
            | o == west = (y, x - 1)
    next | (y',x') `S.member` open = move wrap (open, wall) (y',x',o) (d - 1)
         | (y',x') `S.member` wall = (y, x, o)
         | otherwise = case wrap (open, wall) (y, x, o) of
                         Left _ -> (y, x, o)
                         Right (y',x',o')
                           -> move wrap (open, wall) (y',x',o') (d - 1)

follow :: (Board -> Position -> Either Position Position)
       -> Board -> Path -> Position
follow wrap (open,wall) path = go path start
  where
    start = (\(y,x) -> (y,x,east)) (S.findMin open)

    go [] pos = pos
    go (Turn d:is) (y,x,o) = go is (y, x, turn o d)
    go (Move d:is) pos = go is (move wrap (open, wall) pos d)

password :: Position -> Int
password (y,x,o) = 1000 * y + 4 * x + o

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = password . uncurry (follow wrapFlat) <$> input
  printAnswer "Final password: " answer

wrapCube :: Board -> Position -> Either Position Position
wrapCube (open,wall) pos = eith (y', x', o')
  where
    tiles = S.union open wall
    (y',x',o') = candidate tiles pos
    eith | (y',x') `S.member` wall = Left
         | (y',x') `S.member` open = Right

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = password . uncurry (follow wrapCube) <$> input
  printAnswer "Final password on cube: " answer

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
