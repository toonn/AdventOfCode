module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Data.Char (isLower)
import qualified Data.Map as M
import Data.Monoid (Sum(..))
import qualified Data.Set as S

type Input = M.Map String [String]

deviceName :: Parser String
deviceName = takeWhile1P (Just "Lower case letter") isLower

parser :: Parser Input
parser = M.fromList
     <$> sepEndBy1 ((,) <$> deviceName
                         <* lexeme (char ':')
                        <*> sepEndBy1 deviceName hspace1
                   )
                   eol
      <* eof

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = marf "you" "out" <$> input
  printAnswer "Different paths: " answer

partialInsert :: M.Map String (S.Set String) -> String -> [String] -> [String]
partialInsert _ s [] = [s]
partialInsert order s (x:xs) | Just True <- elem s <$> order M.!? x = s:x:xs
                             | otherwise = x : partialInsert order s xs

marf :: String -> String -> Input -> Int
marf from to connections = getSum . snd
                         $ go (M.fromList [("out",Sum 0),(to,Sum 1)]) from
  where
    go :: M.Map String (Sum Int) -> String -> (M.Map String (Sum Int), Sum Int)
    go counts f
      | Just count <- counts M.!? f = (counts,count)
      | otherwise
      = (\(cs, c) -> (M.insert f c cs, c))
      $ foldl' (\(cs, c) n -> (c +) <$> go cs n)
               (counts, Sum 0)
               (connections M.! f)

-- Too low: 700666418176
part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = sum
             . (\i -> map (\p ->
               product $ zipWith (\f t -> marf f t i) p (drop 1 p)
                          )
                          [ ["svr","fft","dac","out"]
                          , ["svr","dac","fft","out"]
                          ]
               )
           <$> input
  printAnswer "Paths through dac and fft: " answer

main :: IO ()
main = do
  let day = "Day 11: Reactor"
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
