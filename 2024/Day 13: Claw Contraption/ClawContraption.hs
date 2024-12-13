module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Data.Maybe (mapMaybe)

type ClawMachine = ((Int,Int),(Int,Int),(Int,Int))

type Input = [ClawMachine]

xyPair :: Parser (Int, Int)
xyPair = do char 'X'
            oneOf "+="
            x <- integer
            string ", Y"
            oneOf "+="
            y <- integer
            pure (x,y)

clawMachine :: Parser ClawMachine
clawMachine = do string "Button A: "
                 a <- xyPair
                 eol
                 string "Button B: "
                 b <- xyPair
                 eol
                 string "Prize: "
                 prize <- xyPair
                 eol
                 pure (a,b,prize)

parser :: Parser Input
parser = sepEndBy1 clawMachine eol <* eof

abPresses :: ClawMachine -> Maybe (Int,Int)
abPresses ((ax,ay),(bx,by),(px,py))
  = let (b,r1) = (py * ax - ay * px) `quotRem` (by * ax - ay * bx)
        (a,r2) = (px - b * bx) `quotRem` ax
        res | r1 == 0, r2 == 0 = Just (a,b)
            | otherwise = Nothing
     in res

cost :: (Int, Int) -> Int
cost (a,b) = 3 * a + b

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . map cost . mapMaybe abPresses <$> input
  printAnswer "Tokens for all prizes: " answer

adjustPrize :: ClawMachine -> ClawMachine
adjustPrize (a,b,p) = (a,b,both (+ 10000000000000) p)

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = sum . map cost . mapMaybe abPresses . map adjustPrize <$> input
  printAnswer "Tokens for adjusted prizes: " answer

main :: IO ()
main = do
  let day = "Day 13: Claw Contraption"
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
