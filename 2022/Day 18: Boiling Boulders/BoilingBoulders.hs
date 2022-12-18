module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Monad (guard)
import qualified Data.Set as S

import AoC

type Coord = (Int, Int, Int)
type Droplet = S.Set Coord

type Input = Droplet

coordinate :: Parser Coord
coordinate = do
  x <- integer
  char ','
  y <- integer
  char ','
  z <- integer
  pure (x,y,z)

parser :: Parser Input
parser = S.fromList <$> sepEndBy (coordinate) eol <* eof

neighbors :: Coord -> [Coord]
neighbors (x,y,z) = do
  dx <- [-1,0,1]
  dy <- [-1,0,1]
  dz <- [-1,0,1]
  guard (abs dx + abs dy + abs dz == 1)
  pure (x+dx, y+dy, z+dz)

surfaceArea :: Droplet -> Int
surfaceArea droplet = S.foldr (\c area ->
                                area + S.size ( (S.fromAscList (neighbors c))
                                           S.\\ droplet
                                              )
                              )
                              0
                              droplet

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = surfaceArea <$> input
  printAnswer "Surface area of scanned lava droplet: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const "P" <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 18: Boiling Boulders"
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
