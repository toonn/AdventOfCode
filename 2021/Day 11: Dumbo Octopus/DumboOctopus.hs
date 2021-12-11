module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec hiding (chunk)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Char (digitToInt)
import qualified Data.Map as M
import qualified Data.Set as S

import AoC

type Coord = (Int, Int)
type Octopodises = M.Map Coord Int

row :: Parser [(Int, Int)]
row = zip [0..] <$> manyTill (digitToInt <$> digitChar) eol

parser :: Parser Octopodises
parser = M.fromList
       . concat
       . map (\(y, r) -> map (\(x,o) -> ((x,y), o)) r)
       . zip [0..]
     <$> manyTill row eof

increment :: Octopodises -> (S.Set Coord, Octopodises)
increment = M.mapAccumWithKey (\fs c o ->
                                ( if o == 9 then S.insert c fs else fs
                                , o + 1
                                )
                              )
                              S.empty

neighbors :: Coord -> [Coord]
neighbors (x,y) =
  [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]

flash :: S.Set Coord -> S.Set Coord -> Octopodises -> (S.Set Coord, Octopodises)
flash flashed flashing os
  | S.null flashing = (flashed, os)
  | otherwise = flash (S.union flashed flashing) flashing' os'
  where
    flashAdjacent = S.foldr (\c ns -> neighbors c <> ns)
                            []
                            flashing
    (flashing', os') = foldr
      (\c (fs, os'') ->
        case M.lookup c os'' of
          Nothing -> (fs, os'')
          Just o | o == 9    -> (S.insert c fs, M.adjust (+1) c os'')
                 | otherwise -> (           fs, M.adjust (+1) c os'')
      )
      (S.empty, os)
      flashAdjacent

zero :: S.Set Coord -> Octopodises -> Octopodises
zero flashed os = M.union (M.fromSet (const 0) flashed) os

step :: Int -> Octopodises -> (Int, Octopodises)
step 0 os = (0, os)
step n os = (flashes + flashes', os')
  where
    (tens, incremented) = increment os
    (flashed, flashedOs) = flash S.empty tens incremented
    flashes = S.size flashed
    (flashes', os') = step (n-1) (zero flashed flashedOs)

part1 :: Parsed Octopodises -> IO ()
part1 input = do
  let answer = fst . step 100 <$> input
  printAnswer "Flashes after 100 steps: " answer

synchronize :: Octopodises -> Int
synchronize os = go 1 os
  where
    all = M.size os

    go :: Int -> Octopodises -> Int
    go stp os = case step 1 os of
      (fs, os') | fs == all -> stp
                | otherwise -> go (stp + 1) os'

part2 :: Parsed Octopodises -> IO ()
part2 input = do
  let answer = synchronize <$> input
  printAnswer "First synchronized flash: " answer

main :: IO ()
main = do
  let day = "Day 11: Dumbo Octopus"
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
