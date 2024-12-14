module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Control.Arrow ((***))

type Robot = (YX,(Int,Int))

type Input = [Robot]

xyPair :: Parser (Int, Int)
xyPair = do x <- signed integer
            (char ',')
            y <- signed integer
            pure (y,x)

robotPV :: Parser Robot
robotPV = do string "p="
             p <- xyPair
             hspace
             string "v="
             v <- xyPair
             pure (p,v)

parser :: Parser Input
parser = sepEndBy1 robotPV eol <* eof

step :: Int -> Int -> Int -> Robot -> Robot
step ym xm times ((y,x),(vy,vx))
  = ( nTimes times (((`mod` ym) . (+ vy)) *** ((`mod` xm) . (+ vx))) (y,x)
    , (vy,vx)
    )

quadrants :: Int -> Int -> [Robot] -> ([YX],[YX],[YX],[YX])
quadrants ym xm
  = let yb = ym `quot` 2
        xb = xm `quot` 2
     in foldr (\((y,x),_) ->
                \(nw,ne,sw,se) -> case (y `compare` yb, x `compare` xb) of
                 (LT,LT) -> ((y,x):nw,ne,sw,se)
                 (LT,GT) -> (nw,(y,x):ne,sw,se)
                 (GT,LT) -> (nw,ne,(y,x):sw,se)
                 (GT,GT) -> (nw,ne,sw,(y,x):se)
                 _ -> (nw,ne,sw,se)
              )
              ([],[],[],[])

safetyFactor :: ([YX],[YX],[YX],[YX]) -> Int
safetyFactor (nw,ne,sw,se) = length nw * length ne * length sw * length se

-- 104609262 too low (used example width and height on my input)
part1 :: Parsed Input -> IO ()
part1 input = do
  let ym = 103
      xm = 101
      answer = safetyFactor . quadrants ym xm . map (step ym xm 100) <$> input
  printAnswer "Safety factor after 100 s: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 14: Restroom Redoubt"
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
