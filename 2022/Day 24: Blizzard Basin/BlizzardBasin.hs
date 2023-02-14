module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Monad (guard)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import qualified Data.PQueue.Min as PQ

import AoC

type Bound = Int
type Coord = (Int, Int)
type Blizzard = Char
type Blizzards = M.Map Coord [Blizzard]

type Input = (Coord, Coord, Bound, Bound, Blizzards)

singlePosition :: Parser Int
singlePosition = fromJust . elemIndex '.'
             <$> takeWhile1P (Just "Empty or wall") (`elem` "#.")

row :: Parser [Char]
row = between (char '#') (char '#')
        (takeWhile1P (Just "Empty or blizzard") (`elem` ".^v<>"))

parser :: Parser Input
parser = do
  startX <- singlePosition
  eol
  rows <- sepEndBy (try row) eol
  goalX <- singlePosition
  eol
  eof
  let blizzards = M.fromDistinctAscList
                . concat
                . zipWith (\y ->
                            map (\(x,b) -> ((y,x), [b]))
                            . filter ((/= '.') . snd)
                          )
                          [1..]
                . map (zip [1..])
                $ rows
  pure ( (0,startX)               -- start (y,x)
       , (1 + length rows, goalX) -- goal (y,x)
       , length rows              -- max y excepting goal
       , length (head rows)       -- max x
       , blizzards                -- non-empty locations
       )

blizzardStep :: Bound -> Bound -> Coord -> Blizzard -> Coord
blizzardStep boundY boundX (y,x) b
  | b == '^' = let y' | y == 1 = boundY
                      | otherwise = y - 1
                in (y',x)
  | b == 'v' = let y' | y == boundY = 1
                      | otherwise = y + 1
                in (y',x)
  | b == '<' = let x' | x == 1 = boundX
                      | otherwise = x - 1
                in (y,x')
  | b == '>' = let x' | x == boundX = 1
                      | otherwise = x + 1
                in (y,x')

blizzardsStep :: Bound -> Bound -> Blizzards -> Blizzards
blizzardsStep boundY boundX
  = M.foldrWithKey (\coord bs blizzards' ->
                     foldr (\b ->
                             M.insertWith (<>)
                                          (blizzardStep boundY boundX coord b)
                                          [b]
                           )
                           blizzards'
                           bs
                   )
                   M.empty

neighbors :: Coord -> Coord -> Bound -> Bound -> Blizzards -> Coord -> [Coord]
neighbors start end boundY boundX blizzards (y,x) = do
  dy <- [-1,0,1]
  dx <- [-1,0,1]
  guard (dy == 0 || dx == 0)
  let y' = y + dy
  let x' = x + dx
  let n = (y',x')
  guard (1 <= y' && y' <= boundY || n == start || n == end)
  guard (1 <= x' && x' <= boundX)
  guard (n `M.notMember` blizzards)
  pure n

shortestPath :: (Coord, Coord, Bound, Bound, Blizzards)
             -> Maybe Int
shortestPath (start, end, boundY, boundX, blizzards)
  = go candidates shortestPaths
  where
    goal = end
    lowerBound = manhattan start goal
    candidates = PQ.singleton (lowerBound, start, 0, blizzards)
    shortestPaths = M.singleton (start, 0) 0

    period = lcm boundY boundX

    go cs sP
      | Nothing <- mNext = Nothing
      | next == goal = Just d
      | otherwise = go cs'' sP'
      where
        mNext = PQ.minView cs
        Just ((d, next, step, bs), cs') = mNext

        Just shortestHere = M.lookup (next, step) sP
        s = 1 + shortestHere
        step' = s `rem` period
        bs' = blizzardsStep boundY boundX bs
        (cs'', sP') = foldr (\n (cs, sP) -> case M.lookup (n, step') sP of
                              Just s' | s >= s' -> (cs, sP)
                              _ -> ( PQ.insert (s + manhattan n goal
                                               , n
                                               , step'
                                               , bs'
                                               )
                                               cs
                                   , M.insert (n, step') s sP
                                   )
                            )
                            (cs', sP)
                            (neighbors start end boundY boundX bs' next)

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = fromJust . shortestPath Nothing <$> input
  printAnswer "Fewest minutes avoiding blizzards: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const "P" <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 24: Blizzard Basin"
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
