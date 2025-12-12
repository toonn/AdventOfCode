module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Data.Bool (bool)
import Data.Char (intToDigit)
import Data.Foldable (find, fold)
import Data.Function ((&))
import Data.List (nub,intercalate, intersperse)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.SBV as SBV

type Input = [([Int],[[Int]],[Int])]

betwixt :: Char -> Char -> Parser a -> Parser a
betwixt open close = between (char open) (char close)

commaSeparatedNumbers :: Parser [Int]
commaSeparatedNumbers = sepEndBy1 integer (char ',')

diagram :: Parser ([Int])
diagram = map (bool 0 1 . (== '#'))
      <$> betwixt '[' ']' (some (anySingleBut ']'))

schematics :: Parser [[Int]]
schematics = sepEndBy1 (betwixt '(' ')' commaSeparatedNumbers) hspace1

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

part1 :: Parsed Input -> IO ()
part1 input = do
  answer <- either (pure . Left) (Right <$>)
              ( (sum <$>)
              . mapM (\(lightConfiguration, buttons, _) ->
                       solveFor ( (\l s -> s `SBV.sRem` 2 SBV..== l)
                                . fromIntegral
                              <$> lightConfiguration
                                )
                         buttons
                     )
            <$> input
              )
  printAnswer "Fewest presses: " answer

renderButton :: [Int] -> String
renderButton = ('(':) . (<> ")") . intersperse ',' . map intToDigit

solveFor :: [SBV.SInteger -> SBV.SBool] -> [[Int]] -> IO Integer
solveFor joltageRequirements buttons
  = fmap ( fromJust
         . uncurry SBV.getModelValue
         . fromJust
         . find ((== "sum") . fst)
         )
  . SBV.optIndependent
  $ do let buttonNames = map renderButton buttons
       vars <- SBV.sIntegers buttonNames
       let byJoltage = foldr (\(button, var) js ->
                               foldr (M.adjust (var :)) js button
                             )
                             (M.fromList . map (flip (,) []) . nub . fold $ buttons)
                     $ zip buttons vars
       mapM_ SBV.constrain (map (SBV..>= 0) vars)
       mapM_ SBV.constrain (zipWith (&)
                             (M.elems (sum <$> byJoltage))
                             joltageRequirements
                           )
       SBV.minimize "sum" (sum vars)

part2 :: Parsed Input -> IO ()
part2 input = do
  answer <- either (pure . Left)
                   (Right <$>)
                   ( (sum <$>)
                   . mapM (\(_, buttons, joltageRequirements) ->
                            solveFor ((SBV..==) . fromIntegral <$> joltageRequirements)
                              buttons
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
