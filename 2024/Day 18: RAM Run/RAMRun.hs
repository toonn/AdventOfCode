module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import qualified Data.Set as S

type Input = [YX]

coord :: Parser YX
coord = flip (,) <$> integer <* char ',' <*> integer

parser :: Parser Input
parser = sepEndBy1 coord eol <* eof

neighbors :: YX -> S.Set YX -> YX -> [YX]
neighbors (bY,bX) corrupt (y,x) = [ (y', x') | dy <- [-1,0,1]
                                             , dx <- [-1,0,1]
                                             , abs dy /= abs dx
                                             , let y' = y + dy
                                             , let x' = x + dx
                                             , y' >= 0
                                             , y' <= bY
                                             , x' >= 0
                                             , x' <= bX
                                             , S.notMember (y', x') corrupt
                                  ]

minimalSafeSteps :: YX -> YX -> Int -> [YX] -> Int
minimalSafeSteps start end n bytes
  = aStar (neighbors end (S.fromList (take n bytes)))
          manhattan
          (manhattan end)
          (== end)
          start

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = minimalSafeSteps (0,0) (70,70) 1024 <$> input
  printAnswer "Minimum steps: " answer

floodFill :: YX -> S.Set YX -> S.Set YX -> S.Set YX
floodFill end corrupt flooded
  = let flooded' = flooded <> foldMap (S.fromList . neighbors end corrupt)
                                      flooded
        res | flooded == flooded' = flooded
            | otherwise = floodFill end corrupt flooded'
     in res

firstBlocker :: YX -> [YX] -> YX
firstBlocker end bytes
  = foldr (\blocker more (flood, corrupt) ->
            let ns = S.fromList (neighbors end corrupt blocker)
                corrupt' = S.delete blocker corrupt
                flood' | null (flood S.\\ ns) = flood
                       | otherwise = floodFill end corrupt' flood
                res | (0,0) `elem` flood' = blocker
                    | otherwise = more (flood', corrupt')
             in res
          )
          (const (0,0))
          (reverse bytes)
          (S.singleton end, S.fromList bytes)

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = (\(y,x) -> show x <> (',' : show y)) . firstBlocker (70,70)
           <$> input
  printAnswer "First byte that prevents exit: " answer

main :: IO ()
main = do
  let day = "Day 18: RAM Run"
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
