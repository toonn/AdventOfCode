module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Control.Arrow ((&&&))
import Data.Char (digitToInt)
import qualified Data.Map as M
import qualified Data.Set as S

type Input = [[Char]]

parser :: Parser Input
parser = characterGrid <* eof

relate :: M.Map YX Int -> (Int -> Int -> Bool) -> YX -> YX -> Bool
relate terrain op yx1 yx2 = let mH1 = terrain M.!? yx1
                                mH2 = terrain M.!? yx2
                                reachable | Just h1 <- mH1, Just h2 <- mH2
                                          = h1 `op` h2
                                          | otherwise = False
                             in reachable

scending :: (YX -> YX -> Bool) -> YX -> [YX]
scending p (y,x) = filter (p (y,x))
                          [ (y + dy, x + dx) | dy <- [-1,0,1]
                                             , dx <- [-1,0,1]
                                             , abs dy /= abs dx
                          ]

extend :: (YX -> [YX]) -> M.Map YX [[YX]] -> M.Map YX [[YX]]
extend next = M.unionsWith (<>)
            . M.foldrWithKey (\current paths ->
                               ( ( M.fromAscList
                                 . map (\n -> (n, map (n:) paths))
                                 $ next current
                                 )
                               :
                               )
                             )
                             []

stackOn :: [a] -> [a] -> [a]
stackOn [] bottom = bottom
stackOn (h:top) bottom = top `stackOn` (h:bottom)

combine :: M.Map YX [[YX]] -> M.Map YX [[YX]] -> M.Map YX [[YX]]
combine = ( ( M.unionsWith (<>)
            . M.elems
            . fmap (M.unionsWith (<>) . map (\(s:p) -> M.singleton s [(s:p)]))
            )
          .
          )
        . M.intersectionWith (\fP bP -> foldMap (\b -> map (`stackOn` b) fP) bP)

trails :: M.Map YX Int -> M.Map YX [[YX]]
trails terrain = let starts = M.mapWithKey (\k _ -> [[k]])
                                           (M.filter (== 0) terrain)
                     ends = M.mapWithKey (\k _ -> [[k]])
                                         (M.filter (== 9) terrain)
                     a = relate terrain (((== 1) .) . subtract)
                     de = relate terrain (((== 1) .) . (-))
                     forwards = nTimes 5 (extend (a `scending`)) starts
                     backwards = nTimes 4 (extend (de `scending`)) ends
                  in combine forwards backwards

score :: [[YX]] -> Int
score = length . S.fromList . map (head &&& last)

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . M.elems . fmap score . trails . fmap digitToInt . foldYX
           <$> input
  printAnswer "Total trailhead score: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 10: Hoof It"
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
