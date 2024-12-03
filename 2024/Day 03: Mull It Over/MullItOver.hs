module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Data.Maybe (catMaybes)

type Input = [(Int,Int)]

nom :: Parser String
nom = takeWhile1P (Just "Anything but m") (/= 'm')

mulInst :: Parser (Int,Int)
mulInst = string "mul" *> between (char '(') (char ')')
                                  ((,) <$> integer <* char ',' <*> integer)

parser :: Parser Input
parser = catMaybes <$> some ( (try (pure <$> mulInst))
                          <|> (char 'm' *> pure Nothing)
                          <|> (nom *> pure Nothing)
                            )
      <* eof

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . map (uncurry (*)) <$> input
  printAnswer "Sum of multiplications: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 03: Mull It Over"
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
