module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)

data Orientation = N | E | S | W deriving (Show, Eq, Ord)

type POGG = (YX, Orientation)

type Input = [[Char]]

parser :: Parser Input
parser = characterGrid <* eof

move :: Orientation -> YX -> YX
move N (y,x) = (y - 1, x)
move E (y,x) = (y, x + 1)
move S (y,x) = (y + 1, x)
move W (y,x) = (y, x - 1)

turnRight :: Orientation -> Orientation
turnRight N = E
turnRight E = S
turnRight S = W
turnRight W = N

turnLeft :: Orientation -> Orientation
turnLeft N = W
turnLeft E = N
turnLeft S = E
turnLeft W = S

neighbors :: M.Map YX Char -> YX -> POGG -> [POGG]
neighbors grid g (yx, o)
  = let step o' | let yx' = move o' yx, Just t <- grid M.!? yx', t `elem` ".ES"
                = Just (yx', o')
                | otherwise = Nothing
     in mapMaybe step $ [o, turnLeft o, turnRight o]

turns :: Orientation -> Orientation -> Int
turns o o' | o == o' = 0
           | o == turnRight o' || o == turnLeft o' = 1
           | otherwise = 2

distance :: POGG -> POGG -> Int
distance (yx, o) (yx', o') = manhattan yx yx' + 1000 * turns o o'

heuristic :: YX -> POGG -> Int
heuristic g (yx, o) = manhattan yx g + 1000 * ( if fst yx == fst g
                                                || snd yx == snd g
                                                then 0
                                                else 1
                                              )

isGoal :: YX -> POGG -> Bool
isGoal = (. fst) . (==)

findKey :: Eq a => a -> M.Map k a -> k
findKey v = (\(Just (k,_)) -> k) . find ((== v) . snd) . M.assocs

shortestPath :: YX -> YX -> M.Map YX Char -> Int
shortestPath start end grid = aStar (neighbors grid end)
                                    distance
                                    (heuristic end)
                                    (isGoal end)
                                    (start, E)

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = (\grid ->
                 let start = findKey 'S' grid
                     end = findKey 'E' grid
                  in shortestPath start end grid
               )
             . foldYX
           <$> input
  printAnswer "Lowest possible score: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 16: Reindeer Maze"
  input <- readInput day parser
  putStrLn ""
  part1 input
  part2 input
  putStrLn ""
  defaultMainWith (defaultConfig { resamples = 1 }) [
      bgroup "AoC"
        [ bench "Part 1" $ nfIO (silence $ readInput day parser >>= part1)
        , bench "Part 2" $ nfIO (silence $ readInput day parser >>= part2)
        ]
    ]
