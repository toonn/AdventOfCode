module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Data.Bool (bool)
import Data.Function (on)
import qualified Data.Map as M
import qualified Data.Set as S

type Input = [(M.Map Int Int,S.Set (S.Set Int),M.Map Int Int)]

betwixt :: Char -> Char -> Parser a -> Parser a
betwixt open close = between (char open) (char close)

commaSeparatedNumbers :: Parser [Int]
commaSeparatedNumbers = sepEndBy1 integer (char ',')

diagram :: Parser (M.Map Int Int)
diagram = M.fromAscList . zip [0..] . map (bool 0 1 . (== '#'))
      <$> betwixt '[' ']' (some (anySingleBut ']'))

schematics :: Parser (S.Set (S.Set Int))
schematics = S.fromList
         <$> sepEndBy1 (S.fromAscList <$> betwixt '(' ')' commaSeparatedNumbers)
                       hspace1

joltageRequirements :: Parser (M.Map Int Int)
joltageRequirements = M.fromAscList . zip [0..]
                  <$> betwixt '{' '}' commaSeparatedNumbers

parser :: Parser Input
parser = sepEndBy1 ( (,,)
                 <$> diagram
                  <* hspace1
                 <*> schematics
                 <*> joltageRequirements
                   )
                   eol
      <* eof

fewestPresses :: (Ord a, Monoid a)
              => ((a, M.Map Int Int) -> [(a, M.Map Int Int)])
              -> M.Map Int Int
              -> S.Set (S.Set Int)
              -> Int
fewestPresses neighbors target buttons
  = aStar neighbors distance heuristic isGoal start
  where
    distance = ((minimum . M.filter (/= 0)) .) . M.unionWith ((abs .) . (-))
          `on` snd
    heuristic = maximum . snd
    isGoal = all (== 0) . snd
    start = (mempty, target)

lightNeighbors :: S.Set (S.Set Int) -> ((), M.Map Int Int)
               -> [((), M.Map Int Int)]
lightNeighbors buttons (a, configuration)
  = foldr ( (:)
          . (,) ()
          . foldl' (\m k -> M.adjust (bool 0 1 . (== 0)) k m) configuration
          )
          []
          buttons

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum
             . map (\(lights,buttons,_) ->
                     fewestPresses (lightNeighbors buttons) lights buttons
                   )
           <$> input
  printAnswer "Fewest presses: " answer

-- Assumption that at each step there is a button that needs to be pressed as
-- many times as possible seems wrong. There must be a joltage requirement and
-- button schematic combination where at some point you need to press a button
-- fewer times than it could be pressed.
joltageNeighbors :: S.Set (S.Set Int) -> (S.Set (S.Set Int), M.Map Int Int)
                 -> [(S.Set (S.Set Int), M.Map Int Int)]
joltageNeighbors buttons (pressed, joltageLevels)
  = foldr (\button more seen ->
            let mPress | all (> 0) (M.restrictKeys joltageLevels button)
                       = ((seen, M.unionWith (-) joltageLevels
                                                 (M.fromSet (const 1) button)
                          )
                         :
                         )
                       | otherwise = id
             in mPress (more (S.insert button seen))
          )
          (const [])
          (S.toAscList (buttons S.\\ pressed))
          pressed

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = sum
             . map (\(_,buttons,joltageRequirements) ->
                     fewestPresses (joltageNeighbors buttons)
                                   joltageRequirements
                                   buttons
                   )
           <$> input
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
