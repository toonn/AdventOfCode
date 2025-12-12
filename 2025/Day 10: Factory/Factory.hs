{-# OPTIONS_GHC -Wall #-}
module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.SBV as SBV
import qualified Data.Set as S

type Input = [(S.Set Int,[S.Set Int],[Int])]

betwixt :: Char -> Char -> Parser a -> Parser a
betwixt open close = between (char open) (char close)

commaSeparatedNumbers :: Parser [Int]
commaSeparatedNumbers = sepEndBy1 integer (char ',')

diagram :: Parser (S.Set Int)
diagram = S.fromAscList . map fst . filter snd . zip [0..] . map (== '#')
      <$> betwixt '[' ']' (some (anySingleBut ']'))

schematics :: Parser [S.Set Int]
schematics = sepEndBy1 (S.fromAscList <$> betwixt '(' ')' commaSeparatedNumbers)
                       hspace1

joltageRequirements :: Parser [Int]
joltageRequirements = betwixt '{' '}' commaSeparatedNumbers

parser :: Parser Input
parser = sepEndBy1 ( (,,)
                 <$> diagram
                  <* hspace1
                 <*> schematics
                 <*> joltageRequirements
                   )
                   eol
      <* eof

fewestPresses :: (S.Set Int, [S.Set Int], a) -> Int
fewestPresses (lightConfiguration, buttons, _)
  = aStar neighbors distance heuristic isGoal start
  where
    -- symmetricDifference requires containers >= 0.8 :'(
    neighbors c = map (\b -> (c S.\\ b) <> (b S.\\ c)) buttons
    distance _ _ = 1
    heuristic = const 0
    isGoal = null
    start = lightConfiguration

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . map fewestPresses <$> input
  printAnswer "Fewest presses: " answer

solveFor :: [Int] -> [S.Set Int] -> IO Integer
solveFor joltageLevels buttons
  = fmap (fromJust . SBV.getModelValue "sum") . SBV.optLexicographic
  $ do vars <- mapM (const SBV.sInteger_ ) buttons
       let byJoltage = foldr (\(button, var) js ->
                               foldr (M.adjust (var :)) js button
                             )
                             (M.fromAscList . zip [0..] . map (const [])
                             $ joltageLevels
                             )
                     $ zip buttons vars
       mapM_ SBV.constrain (map (SBV..>= 0) vars)
       mapM_ SBV.constrain (zipWith (SBV..==) (M.elems (sum <$> byJoltage))
                                              (fromIntegral <$> joltageLevels)
                           )
       SBV.minimize "sum" (sum vars)

part2 :: Parsed Input -> IO ()
part2 input = do
  answer <- either (pure . Left)
                   (Right <$>)
                   ( (sum <$>)
                   . mapM (\(_, buttons, joltageLevels) ->
                            solveFor joltageLevels buttons
                          )
                 <$> input
                   )
  printAnswer "Fewest presses for joltage requirements: " answer

main :: IO ()
main = do
  let day = "Day 10: Factory"
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
