module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Data.Char (isLower)
import qualified Data.Map as M
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

pathCount :: Input -> Int
pathCount connections = (M.! "out") $ go ["you"] mempty
  where
    go :: [String] -> M.Map String Int -> M.Map String Int
    go [] counts = counts
    go ds counts = go (foldMap (\d -> M.findWithDefault [] d connections) ds)
                      (foldl' (\m d -> M.insertWith (+) d 1 m) counts ds)

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = pathCount <$> input
  printAnswer "Different paths: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

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
