module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Data.Maybe (catMaybes)

data Instruction = Mul Int Int | Do | Dont deriving (Show)

type Input = [Instruction]

nom :: Parser String
nom = takeWhile1P (Just "Anything but m or d") (not . (`elem` "dm"))

mulInst :: Parser (Int,Int)
mulInst = string "mul" *> between (char '(') (char ')')
                                  ((,) <$> integer <* char ',' <*> integer)

instruction :: Parser Instruction
instruction = try (uncurry Mul <$> mulInst)
          <|> try (string "do()" *> pure Do)
          <|> try (string "don't()" *> pure Dont)

parser :: Parser Input
parser = catMaybes <$> some ( (try (pure <$> instruction))
                          <|> (oneOf "dm" *> pure Nothing)
                          <|> (nom *> pure Nothing)
                            )
      <* eof

onlyMul :: [Instruction] -> [Int]
onlyMul = foldr (\i rs -> case i of
                            Mul a b -> a * b : rs
                            _ -> rs
                )
                []

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . onlyMul <$> input
  printAnswer "Sum of multiplications: " answer

enabledMul :: [Instruction] -> [Int]
enabledMul is = foldr (\i more enabled -> case i of
                        Mul a b | enabled -> a * b : more enabled
                        Do -> more True
                        Dont -> more False
                        _ -> more enabled
                      )
                      (const [])
                      is
                      True

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = sum . enabledMul <$> input
  printAnswer "Sum of enabled multiplications: " answer

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
