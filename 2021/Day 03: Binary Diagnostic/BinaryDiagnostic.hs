module Main where

import Criterion.Main
import Data.Maybe (mapMaybe)
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AoC

type Binary = [Char]

binaryNumber :: Parser Binary
binaryNumber = manyTill (char '0' <|> char '1') eol

binaryNumbers :: Parser [Binary]
binaryNumbers = manyTill binaryNumber eof

deriveGamma :: [Binary] -> Binary
deriveGamma numbers = map (\c -> if c <= 0 then '0' else '1') . foldr
  (\bs counts ->
    zipWith (+)
            (map (\b -> if b == '0' then -1 else 1) bs)
            counts
  )
  (map (const 0) (head numbers))
  $ numbers

deriveEpsilonFromGamma :: Binary -> Binary
deriveEpsilonFromGamma = map (\b -> if b == '0' then '1' else '0')

binaryToDecimal :: Binary -> Int
binaryToDecimal = go 0
  where
    go :: Int -> Binary -> Int
    go v [] = v
    go v ('0':bs) = go (2*v) bs
    go v ('1':bs) = go (2*v + 1) bs

part1 :: Parsed [Binary] -> IO ()
part1 input = do
  let answer = (\gamma -> binaryToDecimal gamma
                        * binaryToDecimal (deriveEpsilonFromGamma gamma)
               ) . deriveGamma <$> input
  printAnswer "Power consumption (Γ * ε): " answer

bestMatch :: Char -> (Int -> Int -> Bool) -> [Binary] -> Binary
bestMatch preference comparison numbers = go [] numbers
  where
    go :: Binary -> [Binary] -> Binary
    go match [n] = match <> n
    go match ns = case split  ns of
      (b, ns') -> go (match <> [b]) ns'

    split :: [Binary] -> (Char,[Binary])
    split as =
      case foldr (\(b:bs) (w, (ts,fs)) ->
                   if preference == b
                   then (w - 1, (bs:ts, fs)) else (w + 1, (ts, bs:fs))
                 )
                 (0, ([],[]))
                 as of
        (w, (ts, fs))
          | 0 `comparison` w -> (preference, ts)
          | w `comparison` 0 -> (if preference == '0' then '1' else '0', fs)
          | otherwise -> (preference, ts)

lifeSupportRating :: [Binary] -> Int
lifeSupportRating numbers = oxygenGeneratorRating * cO2ScrubberRating
  where
    oxygenGeneratorRating = binaryToDecimal (bestMatch '1' (>) numbers)
    cO2ScrubberRating = binaryToDecimal (bestMatch '0' (<) numbers)


part2 :: Parsed [Binary] -> IO ()
part2 input = do
  let answer = lifeSupportRating <$> input
  printAnswer "Life support rating: " answer

main :: IO ()
main = do
  let day = "Day 03: Binary Diagnostic"
  let parser = binaryNumbers
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
