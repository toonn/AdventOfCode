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
import Data.Maybe (mapMaybe)

import AoC

type Seeds = [Int]
type Mapping = (Int, Int, Int)
type SourceToTarget = IM.IntervalMap Integer Integer

type Input = (Seeds, [[Mapping]])

seeds :: Parser Seeds
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

(<=..<) :: Int -> Int -> I.Interval Integer
a <=..< b = II.toInterval (finite a II.<=..< finite b)
  where
    finite = ER.Finite . fromIntegral

mappingToInterval :: Mapping -> (I.Interval Integer, Integer)
mappingToInterval (target, source, rangeLength) =
  (source <=..< (source + rangeLength), fromIntegral (target - source))

mkMap :: [Mapping] -> SourceToTarget
mkMap = IM.fromList
      . map mappingToInterval

shiftMap :: Integer -> SourceToTarget -> SourceToTarget
shiftMap shift = IM.map (shift +)
               . IM.mapKeysMonotonic (\x -> x - shift)

seedToLocationMap :: [[Mapping]] -> SourceToTarget
seedToLocationMap = foldr (\tMap sToT ->
                            IM.unions
                          . (\(as,bs,cs) -> bs <> as <> cs)
                          . unzip3
                          . map (\(i,shift) ->
                                  (\(as,overlaps,cs) ->
                                    ( as
                                    , shiftMap shift overlaps
                                    , cs
                                    )
                                  )
                                $ IM.split (I.mapMonotonic (+ shift) i) sToT
                                )
                          $ IM.assocs tMap
                          )
                          (IM.whole 0)
                  . map mkMap

seedLocations :: [[Mapping]] -> Seeds -> [Integer]
seedLocations ms = map (\source ->
                         source
                       + IM.findWithDefault 0 source (seedToLocationMap ms)
                       )
                 . map fromIntegral

closestLocation :: Input -> Integer
closestLocation (s, ms) = minimum . seedLocations ms $ s

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = closestLocation <$> input
  printAnswer "Closest location: " answer

expandRanges :: Seeds -> [I.Interval Integer]
expandRanges [] = []
expandRanges (source:rangeLength:rest) =
  (source <=..< (source + rangeLength)) : expandRanges rest

lowerBoundLocations :: [[Mapping]] -> [I.Interval Integer] -> [Integer]
lowerBoundLocations ms =
  concatMap (\i ->
              let (_, ms', _) = IM.split i (seedToLocationMap ms)
               in mapMaybe (\(i',shift) ->
                             let mLoc | ER.Finite l <- I.lowerBound i'
                                      = Just (l + shift)
                                      | otherwise
                                      = Nothing
                              in mLoc
                           )
                $ IM.assocs ms'
            )

closestActual :: Input -> Integer
closestActual (s, ms) = minimum . lowerBoundLocations ms $ expandRanges s

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = closestActual <$> input
  printAnswer "Closest location of all seeds: " answer

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
