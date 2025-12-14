module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Data.Semigroup (stimes)

type Input = [((Int,Int), [Int])]

regionSpec :: Parser ((Int,Int), [Int])
regionSpec = (,) <$> ((,) <$> integer <* char 'x' <*> integer)
                  <* lexeme (char ':')
                 <*> some integer

parser :: Parser Input
parser = skipSome (try ( integer
                      *> char ':'
                      *> eol
                      *> stimes 3 ( takeWhile1P (Just "Any character") (/= '\n')
                                 *> eol
                                  )
                      *> eol
                       )
                  )
      *> (sepEndBy1 regionSpec eol)
      <* eof

fitTrivial :: (YX, [Int]) -> Bool
fitTrivial ((y, x), shapes) = sum shapes <= (y `quot` 3) * (x `quot` 3)

-- Too low:  440 (right answer for someone else)
-- Too high: 903
part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = length . filter fitTrivial <$> input
  printAnswer "Regions that fit presents: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 12: Christmas Tree Farm"
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
