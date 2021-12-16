module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec hiding (chunk)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Monad (guard)
import Data.Char (digitToInt)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import qualified Data.PQueue.Min as PQ

import AoC

type Coord = (Int, Int)
type Cave = M.Map Coord Int

parser :: Parser Cave
parser = M.fromList
       . concat
       . zipWith (\y -> map (\(x,r) -> ((x,y),r))) [0..]
       . map (zip [0..])
     <$> manyTill (manyTill (digitToInt <$> digitChar) eol) eof

neighbors :: Coord -> [Coord]
neighbors (x,y) = do
  dx <- [-1,0,1]
  dy <- [-1,0,1]
  guard (dx == 0 || dy == 0)
  pure (x+dx,y+dy)

manhattan :: Coord -> Coord -> Int
manhattan (x,y) (x',y') = abs (x - x') + abs (y - y')

shortestPath :: Cave -> Int
shortestPath cave = go candidates
                       shortestPaths
  where
    start = (0,0)
    (goal, _) = M.findMax cave
    lowerBound = manhattan start goal
    candidates = PQ.singleton (lowerBound, start)
    shortestPaths = M.singleton start 0

    go cs sP
      | next == goal = d
      | otherwise = go cs'' sP'
      where
        ((d, next), cs') = PQ.deleteFindMin cs
        ns = mapMaybe (\c -> case ( M.lookup c cave
                                  , M.lookup next sP
                                  ) of
                        (Nothing, _) -> Nothing
                        (Just d', Just s) -> Just (c, s + d')
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

part1 :: Parsed Cave -> IO ()
part1 input = do
  let answer = shortestPath <$> input
  printAnswer "Lowest total risk: " answer

shift :: Int -> Cave -> [Cave]
shift d cave = map (\(dx,dy) -> M.mapKeys (\(x,y) -> (x+dx,y+dy)) cave)
                   [ (dx*shiftXY,dy*shiftXY)
                   | dx <- [0..4], dy <- [0..4], dx + dy == d
                   ]
  where
    shiftXY = case M.findMax cave of
                ((x,_),_) -> x + 1

completeCave :: Cave -> Cave
completeCave cave = M.unions
                  . concat
                  . map (uncurry shift)
                  . zip [0..]
                  . take 9
                  . iterate (M.map (\r -> if r == 9 then 1 else r + 1))
                  $ cave

part2 :: Parsed Cave -> IO ()
part2 input = do
  let answer = shortestPath . completeCave <$> input
  printAnswer "Lowest total risk on full map: " answer

main :: IO ()
main = do
  let day = "Day 15: Chiton"
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
