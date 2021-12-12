module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec hiding (chunk)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Char (isLower)
import qualified Data.Map as M
import qualified Data.Set as S

import AoC

type Cave = String
type Edges = [(Cave, Cave)]
type CaveSystem = M.Map Cave (S.Set Cave)
type Path = [Cave]

cave :: Parser Cave
cave = many letterChar

edge :: Parser (Cave, Cave)
edge = do
  c1 <- cave
  char '-'
  c2 <- cave
  eol
  pure (c1,c2)

parser :: Parser Edges
parser = manyTill edge eof

mirror :: Edges -> Edges
mirror [] = []
mirror ((a,b):cs) = (a,b):(b,a):mirror cs

mapSystem :: Edges -> CaveSystem
mapSystem = foldr (\(a,b) m -> if b == "start"
                             then m
                             else M.insertWith (S.union) a (S.singleton b) m
                  )
                  M.empty

pathsFrom :: S.Set Cave -> Cave -> CaveSystem -> [Path]
pathsFrom visited c caveSystem
  | c == "end" = [[c]]
  | c `S.member` visited = []
  | otherwise = case M.lookup c caveSystem of
    Nothing -> []
    Just cs -> map (c:)
             . concat
             . S.map (\c' -> pathsFrom visited' c' caveSystem)
             . (S.\\ visited)
             $ cs
  where
    visited' | any isLower c = S.insert c visited
             | otherwise = visited

part1 :: Parsed Edges -> IO ()
part1 input = do
  let answer = length . pathsFrom S.empty "start" . mapSystem . mirror <$> input
  printAnswer "Paths that visit small caves at most once: " answer

pathsFromTwice :: Bool -> S.Set Cave -> Cave -> CaveSystem -> [Path]
pathsFromTwice twice visited c caveSystem
  | c == "end" = [[c]]
  | twice && visitedBefore = []
  | otherwise = case M.lookup c caveSystem of
    Nothing -> []
    Just cs -> map (c:)
             . concat
             . S.map (\c' -> pathsFromTwice twice' visited' c' caveSystem)
             . (\s -> if twice then s S.\\ visited else s)
             $ cs
  where
    visitedBefore = c `S.member` visited
    twice' = twice || visitedBefore
    visited' | any isLower c = S.insert c visited
             | otherwise = visited

part2 :: Parsed Edges -> IO ()
part2 input = do
  let answer = length
             . pathsFromTwice False S.empty "start"
             . mapSystem
             . mirror
           <$> input
  printAnswer "Paths visiting a single small cave at most twice: " answer

main :: IO ()
main = do
  let day = "Day 12: Passage Pathing"
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
