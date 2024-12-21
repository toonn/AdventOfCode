module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Data.List (sortBy, stripPrefix)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)

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

waysToDesign :: [String] -> M.Map String Int -> String -> M.Map String Int
waysToDesign _ designWays [] = M.insert "" 1 designWays
waysToDesign towels designWays design
  = foldr (\towel more ws ->
            let ways | Just rest <- stripPrefix towel design
                     , let ws' | M.member rest ws = ws
                               | otherwise = waysToDesign towels ws rest
                     = case ws' M.!? rest of
                         Just w -> M.insertWith (+) design w ws'
                         Nothing -> ws
                     | otherwise = ws
             in more ways
          )
          id
          towels
          designWays

ways :: Input -> [Int]
ways (towels, designs)
  = let ws = foldr (\design more ws ->
                     more (waysToDesign (sortBy (invert compare) towels)
                                        ws
                                        design
                          )
                   )
                   id
                   designs
                   mempty
     in mapMaybe (ws M.!?) designs

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = sum . ways <$> input
  printAnswer "Sum of ways to make designs: " answer

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
