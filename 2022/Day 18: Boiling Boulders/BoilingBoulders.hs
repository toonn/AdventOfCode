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

air :: Coord -> Coord -> Droplet -> Droplet
air (minX,minY,minZ) (maxX,maxY,maxZ) droplet
  = S.fromAscList [ (x,y,z)
                  | x <- [minX..maxX]
                  , y <- [minY..maxY]
                  , z <- [minZ..maxZ]
                  ]
  S.\\ droplet

exteriorVolume :: Droplet -> Droplet
exteriorVolume droplet = go volumeStart S.empty
  where
    xs = S.map (\(x,y,z) -> x) droplet
    minX = S.findMin xs - 1
    maxX = S.findMax xs + 1
    ys = S.map (\(x,y,z) -> y) droplet
    minY = S.findMin ys - 1
    maxY = S.findMax ys + 1
    zs = S.map (\(x,y,z) -> z) droplet
    minZ = S.findMin zs - 1
    maxZ = S.findMax zs + 1

    volumeStart = S.fromAscList [ (x,y,z)
                                | x <- [minX,maxX]
                                , y <- [minY,maxY]
                                , z <- [minZ,maxZ]
                                ]

    airVolume = air (minX,minY,minZ) (maxX,maxY,maxZ) droplet

    inBounds (x,y,z) = minX <= x && x <= maxX
                    && minY <= y && y <= maxY
                    && minZ <= z && z <= maxZ

    go edges seen | S.null edges = seen
                  | otherwise
                  = let seen' = seen `S.union` edges
                        candidates = S.foldr (\c s ->
                                               S.fromAscList
                                                 (filter inBounds (neighbors c))
                                               `S.union` s
                                             )
                                             S.empty
                                             edges
                        edges' = S.intersection (candidates S.\\ seen')
                                                airVolume
                     in go edges' seen'

exteriorSurfaceArea :: Droplet -> Int
exteriorSurfaceArea droplet
  = S.foldr (\c area ->
              area + S.size (S.intersection (S.fromAscList (neighbors c))
                                            exterior
                            )
            )
            0
            droplet
  where
    exterior = exteriorVolume droplet

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = exteriorSurfaceArea <$> input
  printAnswer "Exterior surface area of scanned lava droplet: " answer

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
