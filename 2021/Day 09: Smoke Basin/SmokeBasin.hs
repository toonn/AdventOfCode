module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


import Control.Monad (guard)
import Data.Char (digitToInt)
import Data.List (sortOn, zip5, zip6)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import qualified Data.Set as S

import AoC

type Point = (Int, Int, Int)
type HeightMap = [[Int]]

digits :: Parser [Int]
digits = map digitToInt <$> manyTill digitChar eol

parser :: Parser HeightMap
parser = manyTill digits eof

lowPoints :: HeightMap -> [Point]
lowPoints (r:rs) = go 0 Nothing r rs []
  where
    go y (Just as) bs []      lowPs =
      foldr (\(x, a, b', b, b'') lPs ->
              if b < minimum [a,b',b'']
              then (b,x,y):lPs
              else lPs
            )
            lowPs
            (zip5 [0..] as (10:bs) bs (tail bs <> [10]))
    go y Nothing   bs (cs:rs) lowPs =
      go (y + 1)
         (Just bs)
         cs
         rs
         (foldr (\(x, b', b, b'', c) lPs ->
                  if b < minimum [b',b'',c]
                  then (b,x,y):lPs
                  else lPs
                )
                lowPs
                (zip5 [0..] (10:bs) bs (tail bs <> [10]) cs)
         )
    go y (Just as) bs (cs:rs) lowPs =
      go (y + 1)
         (Just bs)
         cs
         rs
         (foldr (\(x, a, b', b, b'', c) lPs ->
                  if b < minimum [a,b',b'',c]
                  then (b,x,y):lPs
                  else lPs
                )
                lowPs
                (zip6 [0..] as (10:bs) bs (tail bs <> [10]) cs)
         )

riskLevel :: Point -> Int
riskLevel (height, _, _) = height + 1

part1 :: Parsed HeightMap -> IO ()
part1 input = do
  let answer = sum . map riskLevel . lowPoints <$> input
  printAnswer "Sum of low point risk levels: " answer

heightMap :: HeightMap -> M.Map (Int, Int) Int
heightMap rows =
  M.fromList (foldr (\row next y ->
                      filter (\(_, h) -> h /= 9)
                             (zip (zip [0..] (repeat y)) row <> next (y + 1))
                    )
                    (const [])
                    rows
                    0
             )

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x,y) = [(x-1,y), (x,y-1), (x,y+1), (x+1,y)]

higherNeighbors :: M.Map (Int, Int) Int -> Point -> S.Set Point
higherNeighbors hM (h, x, y) =
  S.fromList (mapMaybe (\(x',y') ->
                         case M.lookup (x',y') hM of
                           Nothing -> Nothing
                           Just h' | h' >= h -> Just (h',x',y')
                                   | otherwise -> Nothing
                       )
                       (neighbors (x,y))
             )

basin :: M.Map (Int, Int) Int -> S.Set Point -> S.Set Point -> S.Set Point
basin hM ps qs | S.null qs = ps
               | otherwise = basin hM border (border S.\\ ps)
  where
    border = S.foldr (\q ns -> higherNeighbors hM q `S.union` ns) ps qs

basins :: M.Map (Int, Int) Int -> [Point] -> [S.Set Point]
basins hM lowPs = map ((\s -> basin hM s s) . S.singleton) lowPs

part2 :: Parsed HeightMap -> IO ()
part2 input = do
  let answer = product . take 3 . sortOn negate . map S.size . (\heights -> basins (heightMap heights) (lowPoints heights))
           <$> input
  printAnswer "Product of three largest basin sizes: " answer

main :: IO ()
main = do
  let day = "Day 09: Smoke Basin"
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
