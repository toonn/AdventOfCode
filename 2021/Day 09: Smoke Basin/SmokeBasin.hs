module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Char (digitToInt)
import Data.List (zip4, zip5)

import AoC

type HeightMap = [[Int]]

digits :: Parser [Int]
digits = map digitToInt <$> manyTill digitChar eol

parser :: Parser HeightMap
parser = manyTill digits eof

lowPoints :: HeightMap -> [Int]
lowPoints (r:rs) = go Nothing r rs []
  where
    go (Just as) bs []      lowPs =
      foldr (\(a, b', b, b'') lPs ->
              if b < minimum [a,b',b'']
              then b:lPs
              else lPs
            )
            lowPs
            (zip4 as (10:bs) bs (tail bs <> [10]))
    go Nothing   bs (cs:rs) lowPs =
      go (Just bs) cs rs (foldr (\(b', b, b'', c) lPs ->
                                  if b < minimum [b',b'',c]
                                  then b:lPs
                                  else lPs
                                )
                                lowPs
                                (zip4 (10:bs) bs (tail bs <> [10]) cs)
                         )
    go (Just as) bs (cs:rs) lowPs =
      go (Just bs) cs rs (foldr (\(a, b', b, b'', c) lPs ->
                                  if b < minimum [a,b',b'',c]
                                  then b:lPs
                                  else lPs
                                )
                                lowPs
                                (zip5 as (10:bs) bs (tail bs <> [10]) cs)
                         )

riskLevel :: Int -> Int
riskLevel = (+ 1)

part1 :: Parsed HeightMap -> IO ()
part1 input = do
  let answer = sum . map riskLevel . lowPoints <$> input
  printAnswer "Sum of low point risk levels: " answer

part2 :: Parsed HeightMap -> IO ()
part2 input = do
  let answer = const "P" <$> input
  printAnswer "No answer yet: " answer

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
