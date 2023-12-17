module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Control.Arrow (first)
import Data.Char (digitToInt, isDigit)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M

type Grid = (YX, M.Map YX Int)

type CountDirPlace = (Int, Char, YX)

type Input = [[Char]]

parser :: Parser Input
parser = sepEndBy1 (takeWhile1P (Just "Grid element") isDigit) eol <* eof

mkGrid :: Input -> Grid
mkGrid = first (fst . M.findMax) . dupe . foldYX . map (map digitToInt)

neighbors :: YX -> CountDirPlace -> [CountDirPlace]
neighbors (bY,bX) (sameDirCount, dir, (y,x))
  = mapMaybe (\(d, (y',x')) -> let sameDirCount' | dir == d = sameDirCount + 1
                                                 | otherwise = 1
                                   s | dir /= opposite d
                                     , sameDirCount' <= 3
                                     , 0 <= y' && y' <= bY
                                     , 0 <= x' && x' <= bX
                                     = Just (sameDirCount', d, (y',x'))
                                     | otherwise
                                     = Nothing
                                in s
             )
  $ zip "^<>v"
        [ (y + dy, x + dx) | dy <- [-1..1], dx <- [-1..1], abs dy /= abs dx ]
  where
    opposite '^' = 'v'
    opposite '>' = '<'
    opposite 'v' = '^'
    opposite _   = '>'

heatLoss :: M.Map YX Int -> CountDirPlace -> CountDirPlace -> Int
heatLoss blocks _ (_, _, yx) = blocks M.! yx

manhattenTwist :: YX -> CountDirPlace -> Int
manhattenTwist goal (_, _, yx) = uncurry twist (distance goal yx)
  where
    distance (y,x) (y',x') = (abs (y - y'), abs (x - x'))
    twist a b | a > b = twist b a
              | otherwise = let q = a `quot` 3
                                b' = b - q
                             in a + q + b' + b' `quot` 3

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = (\(goal, blocks) ->
                 aStar (neighbors goal)
                       (heatLoss blocks)
                       (manhattenTwist goal)
                       (\(_, _, p) -> p == goal)
                       (0, '.', (0,0))
               )
             . mkGrid <$> input
  printAnswer "Least heat loss: " answer

ultraNeighbors :: YX -> CountDirPlace -> [CountDirPlace]
ultraNeighbors (bY,bX) (sameDirCount, dir, (y,x))
  = mapMaybe (\(d, (y',x')) -> let sameDirCount' | dir == d = sameDirCount + 1
                                                 | otherwise = 1
                                   s | dir /= opposite d
                                     , sameDirCount' <= 10
                                     , 0 <= y' && y' <= bY
                                     , 0 <= x' && x' <= bX
                                     , dir == d || sameDirCount >= 4
                                     || dir == '.'
                                     = Just (sameDirCount', d, (y',x'))
                                     | otherwise
                                     = Nothing
                                in s
             )
  $ zip "^<>v"
        [ (y + dy, x + dx) | dy <- [-1..1], dx <- [-1..1], abs dy /= abs dx ]
  where
    opposite '^' = 'v'
    opposite '>' = '<'
    opposite 'v' = '^'
    opposite _   = '>'

ultraTwist :: YX -> CountDirPlace -> Int
ultraTwist goal (_, _, yx) = uncurry twist (distance goal yx)
  where
    distance (y,x) (y',x') = (abs (y - y'), abs (x - x'))
    twist a b | a > b = twist b a
              | otherwise = let steps | (q,r) <- b `quotRem` 10
                                      , q > 0 || r >= 4
                                      = q + 1
                                      | otherwise
                                      = 2
                             in b + a + (steps - (a `quot` 4 + 1)) * 4

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = (\(goal, blocks) ->
                 aStar (ultraNeighbors goal)
                       (heatLoss blocks)
                       (ultraTwist goal)
                       (\(_, _, p) -> p == goal)
                       (0, '.', (0,0))
               )
             . mkGrid <$> input
  printAnswer "Least heat loss with an ultra crucible: " answer

main :: IO ()
main = do
  let day = "Day 17: Clumsy Crucible"
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
