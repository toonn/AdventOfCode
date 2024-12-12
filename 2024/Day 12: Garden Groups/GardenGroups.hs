module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Control.Arrow ((&&&))
import qualified Data.Map as M
import qualified Data.Set as S

type Input = [[Char]]

parser :: Parser Input
parser = characterGrid <* eof

segment :: Ord a => M.Map YX a -> M.Map a [S.Set YX]
segment grid = foldr (\(yx, plant) more segments ->
                       let newSeg = S.singleton yx
                           (y,x) = yx
                           overlap = S.fromList [ (y - 1, x), (y, x - 1) ]
                        in more ( M.insertWith
                                  ( const (\segs ->
                                            foldr (\seg more seg' ->
                                                    if S.disjoint overlap seg
                                                    then seg : more seg'
                                                    else more (seg <> seg')
                                                  )
                                                  (:[])
                                                  segs
                                                  newSeg
                                          )
                                  )
                                  plant
                                  [newSeg]
                                  segments
                                )
                     )
                     id
                     (M.assocs grid)
                     mempty

area :: S.Set YX -> Int
area = length

neighbors :: YX -> S.Set YX
neighbors (y,x) = S.fromList [ (y + dy, x + dx) | dy <- [-1,0,1]
                                                , dx <- [-1,0,1]
                                                , abs dy /= abs dx
                             ]

interstitial :: S.Set YX -> Int
interstitial seg | Just (yx, seg') <- S.minView seg
                 = length (S.intersection (neighbors yx) seg')
                 + interstitial seg'
                 | otherwise = 0

perimeter :: S.Set YX -> Int
perimeter seg = 4 * area seg - 2 * interstitial seg

price :: S.Set YX -> Int
price seg = area seg * perimeter seg

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . map price . mconcat . M.elems . segment . foldYX <$> input
  printAnswer "Total fencing price: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 12: Garden Groups"
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
