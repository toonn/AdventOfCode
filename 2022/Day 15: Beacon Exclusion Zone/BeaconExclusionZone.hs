module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Map as M
import qualified Data.Set as S

import AoC

type Coord = (Int, Int)
type Sensors = M.Map Coord Coord
type Exclusion = S.Set Coord

type Input = Sensors

sensor :: Parser (Coord, Coord)
sensor = do
  string "Sensor at x="
  sX <- signed integer
  string ", y="
  sY <- signed integer
  string ": closest beacon is at x="
  bX <- signed integer
  string ", y="
  bY <- signed integer
  pure ((sX,sY), (bX,bY))

parser :: Parser Input
parser = M.fromList <$> manyTill (sensor <* eol) eof

reach :: Int -> Sensors -> Sensors
reach y = M.filterWithKey (\(x',y') b ->
                            let r = manhattan (x',y') b
                             in y' - r <= y && y <= y' + r
                          )

exclusionSphere :: Int -> Coord -> Coord -> Exclusion
exclusionSphere y (x',y') b = S.fromAscList . map (\dx -> (x' + dx,y)) $ [-d..d]
  where
    d = manhattan (x',y') b - abs (y - y')

beaconExclusion :: Int -> Sensors -> Exclusion
beaconExclusion y sensors = ( S.unions
                            $ M.foldrWithKey (\s b es ->
                                               exclusionSphere y s b : es
                                             )
                                             []
                                             (reach y sensors)
                            )
                       S.\\ (S.fromList . filter pred . M.elems $ sensors)
  where
    pred  = (== y) . snd

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = S.size . beaconExclusion 2000000 <$> input
  printAnswer "Positions excluding beacon when y = 2M: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const "P" <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 15: Beacon Exclusion Zone"
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
