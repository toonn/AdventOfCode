module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Char (digitToInt)
import qualified Data.IntMap as IM
import Data.List (transpose, zipWith4)
import qualified Data.Set as S
import Data.Tuple (swap)

import AoC

type Tree = Int
type Row = [Tree]
type Grid = [Row]
type Coord = (Int, Int)

type Input = Grid

row :: Parser Row
row = manyTill (digitToInt <$> digitChar) eol

parser :: Parser Grid
parser = manyTill row eof

visibleInRows :: Grid -> S.Set Coord
visibleInRows grid =
  foldr (\r coords y ->
          S.union ( S.fromList
                  . map (\x -> (x,y))
                  $ foldr (\t xCoords x ts ->
                            let x' = x + 1
                                tx = [(t,x)]
                                (h,_)
                                  | (th:_) <- ts = th
                                  | otherwise =
                                    (-1, error "No coordinate for a fake tree")
                                cs | t > h = case xCoords x' tx of
                                               (xC:xs) | x == xC -> x:xs
                                               xs -> x:xs
                                   | otherwise = xCoords x'
                                                         (filter (\(t',_) ->
                                                                   t' > t
                                                                 )
                                                                 ts
                                                        <> tx
                                                         )
                             in cs
                          )
                          (const (map snd))
                          r
                          0
                          []
                  )
                  (coords (y + 1))
        )
        (const S.empty)
        grid
        0

visibleTrees :: Grid -> S.Set Coord
visibleTrees grid = visibleInRows grid
          `S.union` (S.map swap (visibleInRows (transpose grid)))

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = S.size . visibleTrees <$> input
  printAnswer "Visible trees from outside the grid: " answer

leftViewable :: Grid -> [[Int]]
leftViewable = map (\r -> foldr (\t more viewable ->
                                  (viewable IM.! t)
                                  : more (IM.mapWithKey (\k n ->
                                                          if t >= k
                                                          then 1
                                                          else 1 + n
                                                        )
                                                        viewable
                                         )
                                )
                                (\_ -> [])
                                r
                                (IM.fromAscList [(i,0) | i <- [0..9]])
                   )

scenicScores :: Grid -> [[Int]]
scenicScores grid = zipWith4 (zipWith4 (\a b c d -> a * b * c * d))
                             (leftViewable grid)
                             (map reverse . leftViewable . map reverse $ grid)
                             (transpose . leftViewable . transpose $ grid)
                             (transpose . map reverse . leftViewable . map reverse . transpose $ grid)

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = maximum . concat . scenicScores <$> input
  printAnswer "Highest scenic score: " answer

main :: IO ()
main = do
  let day = "Day 08: Treetop Tree House"
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
