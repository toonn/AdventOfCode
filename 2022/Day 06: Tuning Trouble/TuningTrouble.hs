module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Char (isLetter)

import AoC

type Input = String

parser :: Parser Input
parser = takeWhileP (Just "Datastream") isLetter <* eol <* eof

marker :: Int -> Input -> String
marker markerLength datastream
  = foldr (\c more (cs, prelude) ->
             case span (/= c) cs of
               (cs', []) | length cs' == markerLength - 1 -> prelude [c]
                         | otherwise -> more (cs' <> [c], prelude . (c:))
               (_, _:cs') -> more (cs' <> [c], prelude . (c:))
          )
          (($ []) . snd)
          datastream
          ([], id)


packetPrelude :: Input -> String
packetPrelude = marker 4

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = length . packetPrelude <$> input
  printAnswer "Characters processed until start-of-packet: " answer

messagePrelude :: Input -> String
messagePrelude = marker 14

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = length . messagePrelude <$> input
  printAnswer "Characters processed before first start-of-message: " answer

main :: IO ()
main = do
  let day = "Day 06: Tuning Trouble"
  input <- readInput day parser
  putStrLn ""
  part1 input
  part2 input
  putStrLn ""
  defaultMain [
      bgroup "AoC"
        [ bench "Part 1" $ nfIO (silence $ readInput day parser >>= part1)
        , bench "Part 2" $ nfIO (silence $ readInput day parser >>= part2)
        ]
    ]
