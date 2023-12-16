module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Control.Arrow ((&&&), (***), (^>>), (>>^))
import Control.Monad (join)
import qualified Data.Map as M

type Grid = (YX, M.Map YX Char)

type Input = [[Char]]

parser :: Parser Input
parser = sepEndBy1 (takeWhile1P (Just "Grid element") (`elem` "./\\|-")) eol
      <* eof

mkGrid :: Input -> Grid
mkGrid = (fst . M.findMax &&& M.filter (/= '.')) . foldYX

path :: YX -> Char -> YX -> YX -> M.Map YX [Char]
path (bY,bX) dir a b
  | ((yMin,yMax),(xMin,xMax))
  <- both (uncurry min &&& uncurry max) . (both fst &&& both snd) $ (a, b)
  = M.fromAscList [((y,x), [dir]) | y <- [yMin..yMax]
                                  , 0 <= y && y <= bY
                                  , x <- [xMin..xMax]
                                  , 0 <= x && x <= bX
                  ]

encounter :: Char -> YX -> Char -> [(Char, YX)]
encounter '>' (y,x) o | '/'  <- o = [('^', (y - 1, x))]
                      | '\\' <- o = [('v', (y + 1, x))]
                      | '-'  <- o = [('>', (y, x + 1))]
                      | '|'  <- o = [('^', (y - 1, x)), ('v', (y + 1, x))]
encounter 'v' (y,x) o | '/'  <- o = [('<', (y, x - 1))]
                      | '\\' <- o = [('>', (y, x + 1))]
                      | '-'  <- o = [('<', (y, x - 1)), ('>', (y, x + 1))]
                      | '|'  <- o = [('v', (y + 1, x))]
encounter '<' (y,x) o | '/'  <- o = [('v', (y + 1, x))]
                      | '\\' <- o = [('^', (y - 1, x))]
                      | '-'  <- o = [('<', (y, x - 1))]
                      | '|'  <- o = [('^', (y - 1, x)), ('v', (y + 1, x))]
encounter '^' (y,x) o | '/'  <- o = [('>', (y, x + 1))]
                      | '\\' <- o = [('<', (y, x - 1))]
                      | '-'  <- o = [('<', (y, x - 1)), ('>', (y, x + 1))]
                      | '|'  <- o = [('^', (y - 1, x))]

beam :: Grid -> Char -> YX -> M.Map YX [Char] -> M.Map YX [Char]
beam (border, grid) dir (y,x) visited
  | y < 0 || x < 0
  = visited
  | (bY,bX) <- border, y > bY || x > bX
  = visited
  | Just ds <- visited M.!? (y,x)
  , o <- M.findWithDefault '.' (y,x) grid
  , o `elem` "-|"
  || (o == '.'  && dir `elem` "<>" && any (`elem` "<>") ds)
  || (o == '.'  && dir `elem` "^v" && any (`elem` "^v") ds)
  || (o == '/'  && dir `elem` ">v" && any (`elem` ">v") ds)
  || (o == '/'  && dir `elem` "^<" && any (`elem` "^<") ds)
  || (o == '\\' && dir `elem` ">^" && any (`elem` ">^") ds)
  || (o == '\\' && dir `elem` "v<" && any (`elem` "v<") ds)
  = visited
  | otherwise
  = let checkCoord | dir `elem` "<>" = (== y) . fst
                   | otherwise       = (== x) . snd
        g = M.filterWithKey (const . checkCoord) grid
        mO | dir `elem` ">v" = M.lookupGE (y,x) g
           | otherwise = M.lookupLE (y,x) g
        yx' | Just (yx,_) <- mO = yx
            | dir == '>' = (y, snd border + 1)
            | dir == 'v' = (fst border + 1, x)
            | dir == '<' = (y, -1)
            | dir == '^' = (-1, x)
        visited' = M.unionWith (<>) visited (path border dir (y,x) yx')
        visitedNext | Just (yxNext, o) <- mO
                    = foldr ((.) . uncurry (beam (border, grid)))
                            id
                            (encounter dir yxNext o)
                            visited'
                    | otherwise = visited'
     in visitedNext

energized :: Char -> YX -> Grid -> Int
energized dir yx grid = M.size . beam grid dir yx $ M.empty

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = energized '>' (0,0) . mkGrid <$> input
  printAnswer "Energized tiles: " answer

startingConfigurations :: Grid -> [(Char, YX)]
startingConfigurations ((bY,bX), _) =
  [('>', (y,0)) | y <- [0..bY] ]
  <> [('<', (y,bX)) | y <- [0..bY] ]
  <> [('v', (0,x)) | x <- [0..bX] ]
  <> [('^', (bY,x)) | x <- [0..bX] ]

maximumPower :: Grid -> Int
maximumPower grid = maximum
                  . map (($ grid) . uncurry energized)
                  $ startingConfigurations grid

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = maximumPower . mkGrid <$> input
  printAnswer "Largest number of tiles energized: " answer

main :: IO ()
main = do
  let day = "Day 16: The Floor Will Be Lava"
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
