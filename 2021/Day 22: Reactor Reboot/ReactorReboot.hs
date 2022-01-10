module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec hiding (chunk)
import Text.Megaparsec.Char

import Data.Maybe (mapMaybe)
import qualified Data.Set as S

import AoC

type Range = (Int, Int)
type RebootStep = Either (Range, Range, Range) (Range, Range, Range)
type RebootSteps = [RebootStep]
type Coord = (Int, Int, Int)
type Grid = S.Set Coord
type RebootSet = Either Grid Grid

range :: Parser Range
range = do
  char '='
  l <- signed integer
  string ".."
  r <- signed integer
  pure (min l r, max l r)

rebootStep :: Parser RebootStep
rebootStep = do
  onOff <- string "on" <|> string "off"
  let lR | onOff == "on" = Left
         | otherwise = Right
  hspace
  char 'x'
  xRange <- range
  string ",y"
  yRange <- range
  string ",z"
  zRange <- range
  eol
  pure $ lR (xRange, yRange, zRange)

parser :: Parser RebootSteps
parser = manyTill rebootStep eof

filterRange :: Int -> Int -> RebootSteps -> RebootSteps
filterRange l r = mapMaybe (either ((Left <$>) . constrain)
                                   ((Right <$>) . constrain)
                           )
  where
    constrain ((xl,xr),(yl,yr),(zl,zr))
      | xl' <- max l xl , xr' <- min r xr
      , yl' <- max l yl , yr' <- min r yr
      , zl' <- max l zl , zr' <- min r zr
      , xl' <= xr'
      , yl' <= yr'
      , zl' <= zr'
      = Just ((xl',xr'),(yl',yr'),(zl',zr'))
      | otherwise = Nothing

rangeSet :: (Range,Range,Range) -> Grid
rangeSet ((xl,xr),(yl,yr),(zl,zr)) =
  S.fromAscList [(x,y,z) | x <- [xl..xr], y <- [yl..yr], z <- [zl..zr]]

rangesToSets :: RebootSteps -> [RebootSet]
rangesToSets = map (either (Left . rangeSet)
                           (Right . rangeSet)
                   )

foldSets :: [RebootSet] -> Grid
foldSets steps = foldr (\e next grid ->
                         next (either (S.union) (flip S.difference) e grid)
                       )
                       id
                       steps
                       S.empty

part1 :: Parsed RebootSteps -> IO ()
part1 input = do
  let answer = S.size . foldSets . rangesToSets . filterRange (-50) 50 <$> input
  printAnswer "Number of cubes that are on: " answer

part2 :: Parsed RebootSteps -> IO ()
part2 input = do
  let answer = const "P" <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 22: Reactor Reboot"
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
