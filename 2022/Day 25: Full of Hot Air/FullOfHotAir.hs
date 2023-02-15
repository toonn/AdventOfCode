module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.List (foldl')

import AoC

newtype SNAFU = SNAFU [Int]

toChar :: Int -> Char
toChar (-2) = '='
toChar (-1) = '-'
toChar   0  = '0'
toChar   1  = '1'
toChar   2  = '2'
toChar x = error (show x)

instance Show SNAFU where
  show (SNAFU n) = map toChar n

type Input = [SNAFU]

toDigit :: Char -> Int
toDigit '=' = -2
toDigit '-' = -1
toDigit '0' = 0
toDigit '1' = 1
toDigit '2' = 2

number :: Parser SNAFU
number = SNAFU
       . map toDigit
     <$> takeWhile1P (Just "SNAFU digit") (`elem` "=-012")

parser :: Parser Input
parser = sepEndBy number eol <* eof

-- (Carry, Digit) pairs for all possible results, -5..5 (2 + 2 + carry of 1)
-- (-1 0) (-1 1) (-1 2) -2 -1 0 1 2 (1 -2) (1 -1) (1 0)
halfAdd :: Int -> Int -> Int -> (Int, Int)
halfAdd carry a b = let s = carry + a + b
                        sign = signum s
                        v = abs s
                        carry' | v >= 3 = sign
                               | otherwise = 0
                        r | v == 3 = negate (sign * 2)
                          | v == 4 = negate sign
                          | v == 5 = 0
                          | otherwise = sign * v
                     in (carry', r)

add :: SNAFU -> SNAFU -> SNAFU
add (SNAFU term1) (SNAFU term2) = SNAFU
                                . reverse
                                $ go 0 (reverse term1) (reverse term2)
  where
    go 0 [] [] = []
    go carry [] [] = [carry]
    go carry (m1:ert1) [] = let (carry', digit) = halfAdd carry m1 0
                             in digit : go carry' ert1 []
    go carry [] mert2 = go carry mert2 []
    go carry (m1:ert1) (m2:ert2) = let (carry', digit) = halfAdd carry m1 m2
                                    in digit : go carry' ert1 ert2

summation :: [SNAFU] -> SNAFU
summation = foldl' add (SNAFU [0])

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = summation <$> input
  printAnswer "SNAFU number for Bob's console: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const "A very young elf found the 50th star fruit" <$> input
  printAnswer "Psych: " answer

main :: IO ()
main = do
  let day = "Day 25: Full of Hot Air"
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
