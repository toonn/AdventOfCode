module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Monad (guard)
import Data.Char (isLetter)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import qualified Data.PQueue.Min as PQ

import AoC

type Coord = (Int, Int)
type Height = Int
type HeightMap = M.Map Coord Height

type Input = (Coord, Coord, HeightMap)

row :: Parser [(Int, Char)]
row = zip [0..] <$> takeWhileP (Just "Height row") isLetter

parser :: Parser Input
parser = do
  rows <- manyTill (row <* eol) eof
  let heightMap = M.unions
                . map M.fromAscList
                . zipWith (\y -> map (\(x,h) -> ((x, y), h))) [0..]
                $ rows
  let s = M.foldrWithKey (\c h c' -> if h == 'S' then c else c') (0,0) heightMap
  let e = M.foldrWithKey (\c h c' -> if h == 'E' then c else c') (0,0) heightMap
  pure (s, e, M.map (fromEnum) . M.insert s 'a' . M.insert e 'z' $ heightMap)

neighbors :: Coord -> [Coord]
neighbors (x,y) = do
  dx <- [-1,0,1]
  dy <- [-1,0,1]
  guard (abs dx /= abs dy)
  pure (x+dx,y+dy)

shortestPath :: (Coord, Coord, HeightMap) -> Int
shortestPath (start, end, heightMap) = go candidates shortestPaths
  where
    goal = end
    lowerBound = manhattan start goal
    candidates = PQ.singleton (lowerBound, start)
    shortestPaths = M.singleton start 0

    go cs sP
      | next == goal = d
      | otherwise = go cs'' sP'
      where
        ((d, next), cs') = PQ.deleteFindMin cs
        ns = let shortestHere = M.lookup next sP
                 Just h = M.lookup next heightMap
              in mapMaybe (\c -> case (M.lookup c heightMap, shortestHere) of
                            (Nothing, _) -> Nothing
                            (Just h', Just s) | h' - h <= 1 -> Just (c, s + 1)
                                              | otherwise -> Nothing
                          )
                          (neighbors next)

        (cs'', sP') = foldr (\(n, s) (cs, sP) -> case M.lookup n sP of
                              Just s' | s >= s' -> (cs, sP)
                              _ -> ( PQ.insert (s + manhattan n goal, n) cs
                                   , M.insert n s sP
                                   )
                            )
                            (cs', sP)
                            ns

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = shortestPath <$> input
  printAnswer "Fewest steps to best signal location: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const "P" <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 12: Hill Climbing Algorithm"
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
