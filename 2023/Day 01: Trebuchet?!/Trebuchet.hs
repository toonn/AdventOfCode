module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Applicative (asum)
import Data.Char (digitToInt, isAlphaNum, isDigit)
import Data.List (find)
import Data.Maybe (fromMaybe, fromJust)

import AoC

type Line = String
type Input = [Line]

line :: Parser Line
line = takeWhileP (Just "character") isAlphaNum

parser :: Parser Input
parser = manyTill (line <* optional eol) eof

calibrationValues :: Line -> Int
calibrationValues l = (\(t,u) -> 10*t + u)
                    . foldr (\c more (first, last) ->
                              let d = digitToInt c
                                  fl' | isDigit c = (first . const d,  d)
                                      | otherwise = (first, last)
                               in more fl'
                            )
                            (\(first,last) -> (first 0, last))
                            l
                    $ (id,0)

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . map calibrationValues <$> input
  printAnswer "Sum of calibration values: " answer

unspellDigit :: String -> String
unspellDigit "one"   = "1"
unspellDigit "two"   = "2"
unspellDigit "three" = "3"
unspellDigit "four"  = "4"
unspellDigit "five"  = "5"
unspellDigit "six"   = "6"
unspellDigit "seven" = "7"
unspellDigit "eight" = "8"
unspellDigit "nine"  = "9"

unspellPostfix :: Line -> String -> Maybe Line
unspellPostfix initials postfix =
  case splitAt (length initials - length postfix) initials of
    (is, postfix') | postfix == postfix' -> Just (is <> (unspellDigit postfix))
    _ -> Nothing

unspellForward :: Line -> Line
unspellForward l = foldr (\c more initials ->
                          let initials' = initials <> [c]
                           in more
                            . fromMaybe initials'
                            $ ( asum
                              . map (unspellPostfix initials')
                              $ [ "one"
                                , "two"
                                , "three"
                                , "four"
                                , "five"
                                , "six"
                                , "seven"
                                , "eight"
                                , "nine"
                                ]
                              )
                        )
                        id
                        l
                        ""

firstDigit :: Line -> Int
firstDigit = digitToInt . fromMaybe '0' . find isDigit . unspellForward

lastDigit :: Line -> Int
lastDigit l = digitToInt
            . fromMaybe '0'
            . fst
            . foldr (\c (last, rest) ->
                      let (c', rest') | "one" == take 3 (c:rest)
                                      = ('1', drop 3 rest)
                                      | "two" == take 3 (c:rest)
                                      = ('2', drop 3 rest)
                                      | "three" == take 5 (c:rest)
                                      = ('3', drop 5 rest)
                                      | "four" == take 4 (c:rest)
                                      = ('4', drop 4 rest)
                                      | "five" == take 4 (c:rest)
                                      = ('5', drop 4 rest)
                                      | "six" == take 3 (c:rest)
                                      = ('6', drop 3 rest)
                                      | "seven" == take 5 (c:rest)
                                      = ('7', drop 5 rest)
                                      | "eight" == take 5 (c:rest)
                                      = ('8', drop 5 rest)
                                      | "nine" == take 4 (c:rest)
                                      = ('9', drop 4 rest)
                                      | otherwise
                                      = (c, rest)
                          last' | last == Nothing && isDigit c' = Just c'
                                | otherwise = last
                       in (last', c':rest')
                    )
                    (Nothing, "")
            $ l

spelledCalibrationValues :: Line -> Int
spelledCalibrationValues l = 10 * firstDigit l + lastDigit l

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = sum . map spelledCalibrationValues <$> input
  printAnswer "Sum of unspelled calibration values: " answer

main :: IO ()
main = do
  let day = "Day 01: Trebuchet?!"
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
