module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Data.List (sortBy, stripPrefix)

type Input = ([String], [String])

towelPattern :: Parser String
towelPattern = takeWhile1P (Just "A towel pattern") (`elem` "wubrg")

parser :: Parser Input
parser = (,)
     <$> sepBy1 towelPattern (lexeme (char ',')) <* eol <* eol
     <*> sepEndBy1 towelPattern eol <* eof

possibleDesign :: [String] -> String -> Bool
possibleDesign towels [] = True
possibleDesign towels design
  = foldr (\towel next ->
            let possible | Just rest <- stripPrefix towel design
                         , possibleDesign towels rest
                         = True
                         | otherwise = next
             in possible
          )
          False
          towels

invert :: (a -> a -> Ordering) -> (a -> a -> Ordering)
invert comp a b = case comp a b of
                    LT -> GT
                    EQ -> EQ
                    GT -> LT

possibleDesigns :: Input -> [String]
possibleDesigns (towels, designs)
  = filter (possibleDesign (sortBy (invert compare) towels)) designs

-- 309 too low
part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = length . possibleDesigns <$> input
  printAnswer "Possible designs: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 19: Linen Layout"
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
