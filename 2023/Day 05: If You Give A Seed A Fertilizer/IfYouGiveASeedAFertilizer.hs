module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.ExtendedReal as ER
import qualified Data.IntegerInterval as II
import qualified Data.Interval as I
import qualified Data.IntervalMap.Lazy as IM

import AoC

type Seed = Int
type Mapping = (Int, Int, Int)

type Input = ([Seed], [[Mapping]])

seeds :: Parser [Seed]
seeds = lexeme (string "seeds:") *> many integer

mapping :: Parser Mapping
mapping = do
  target <- integer
  source <- integer
  rangeLength <- integer
  pure (target, source, rangeLength)

mappings :: Parser [Mapping]
mappings = do
  source <- takeWhile1P (Just "source") (/= '-')
  string "-to-"
  target <- takeWhile1P (Just "target") (/= ' ')
  string " map:"
  eol
  ms <- sepEndBy (mapping) eol
  pure ms

parser :: Parser Input
parser = do
  s <- seeds
  eol
  eol
  ms <- sepBy mappings eol
  eof
  pure (s, ms)

mappingToInterval :: Mapping -> (I.Interval Integer, Integer -> Integer)
mappingToInterval (target, source, rangeLength) =
  (source <=..< (source + rangeLength), (fromIntegral (target - source) +))
  where
    finite = ER.Finite . fromIntegral

    (<=..<) :: Int -> Int -> I.Interval Integer
    a <=..< b = II.toInterval (finite a II.<=..< finite b)

mkMap :: [Mapping] -> IM.IntervalMap Integer (Integer -> Integer)
mkMap = (<> (IM.whole id))
      . IM.fromList
      . map mappingToInterval

closestLocation :: Input -> Integer
closestLocation (s, ms) = minimum
                        $ foldr (\sToT more sources ->
                                  more
                                . map (\source ->
                                        IM.findWithDefault id source sToT
                                      $ source
                                      )
                                $ sources
                                )
                                id
                                (map mkMap ms)
                                (map fromIntegral s)

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = closestLocation <$> input
  printAnswer "Closest location: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 05: If You Give A Seed A Fertilizer"
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
