module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Control.Arrow ((&&&))
import Control.Monad (join)
import Data.Foldable (foldMap')
import Data.List (sortOn, tails)
import qualified Data.Set as S

type Input = [YX]

parser :: Parser Input
parser = sepEndBy1 ((,) <$> integer <* char ',' <*> integer) eol <* eof

area :: YX -> YX -> Int
area (y1,x1) (y2,x2) = (abs (y1 - y2) + 1) * (abs (x1 - x2) + 1)

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = foldr (\(xy:xys) -> max . maximum . map (area xy) $ xys) 0
             . filter atLeastTwo
             . tails
           <$> input
  printAnswer "Largest rectangle: " answer

atLeastTwo :: [a] -> Bool
atLeastTwo []  = False
atLeastTwo [_] = False
atLeastTwo _   = True

findTip :: [(YX,YX)] -> [(YX,Int)]
findTip = sortOn (snd . fst) -- Sort left before right
        . map (uncurry max &&& (fst . uncurry min)) -- Sort tip first in pairs
        . take 2
        . sortOn (negate . uncurry manhattan) -- Pairs with largest distance

addXBound :: [(YX,Int)] -> [(YX,YX)] -> [(YX,YX)]
addXBound [(l@(y1,_),ly),(r,ry)] pairs = [(l,(ly,lx)),(r,(ry,rx))]
  where
    xs = foldMap ((\(a,b) -> [a,b]) . both snd)
       . filter (\((y,_),(y',_)) -> min y y' <= y1 && y1 <= max y y')
       $ pairs
    lx = minimum xs
    rx = maximum xs

contains :: YX -> YX -> YX -> Bool
contains (y1,x1) (y2,x2) (y,x) = min y1 y2 <= y && y <= max y1 y2
                              && min x1 x2 <= x && x <= max x1 x2

-- Plotting debug_rectangles.png showed that I was computing the "inside"
-- wrong, always subtracting from the tip point and adding to the opposite
-- point.
toward :: YX -> YX -> YX
toward (ty,tx) (y,x) = let y' | ty < y = y - 1
                              | y < ty = y + 1
                              | otherwise = y
                           x' | tx < x = x - 1
                              | x < tx = x + 1
                              | otherwise = x
                        in (y',x')


biggestRectangle :: Input -> (YX,YX) -> Int
biggestRectangle tiles (corner,bound) = area corner biggest
  where
    candidates = filter (/= corner)
               . filter (contains corner bound)
               $ tiles
    (biggest:_) = filter ( \c -> not
                               . any (contains (toward c corner)
                                               (toward corner c)
                                     )
                               . filter (/= c)
                               $ candidates
                         )
                . sortOn (negate . area corner)
                $ candidates

-- A general solution seems out of reach for me so on a hint in
-- #adventofcode@Libera.Chat I plotted the input in GNUplot.
--
-- Produced input.png with
-- ```gnuplot
-- set datafile separator comma
-- plot "input.txt" using 2:1 with lines
-- ```
-- The plot shows an ellipse with a rectangular cut almost across the middle.
--
-- This is a very suspicious structure, the points on the tip of the ellipse
-- are likely corners of the biggest rectangles in the left and right halves,
-- respectively.
--
-- Plan: 1. Find the tip of the cut
--       2. Find the bounds on the corresponding corner
--       3. Find the biggest rectangles in the left and right halves
--       4. Return the bigger of the two
--
-- Too low: 1426288185
part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = maximum -- 4.
             . (\tiles@(t1:_) ->
                 map (biggestRectangle tiles) -- 3.
               . join ( addXBound -- Second part of 2.
                      . findTip -- 1. and part of 2. [(left,lr),(right,ll)]
                      )
               . join (zip . drop 1)
               $ tiles
               )
           <$> input
  printAnswer "Largest red and green rectangle: " answer

main :: IO ()
main = do
  let day = "Day 09: Movie Theater"
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
