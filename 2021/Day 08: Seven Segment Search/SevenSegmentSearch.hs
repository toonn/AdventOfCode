module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Set as S

import AoC

type Digit = S.Set Char
type SignalPattern = ([Digit], [Digit])

digit :: Parser Digit
digit = S.fromList <$> lexeme (many (satisfy (`elem` "abcdefg")))

signalPattern :: Parser SignalPattern
signalPattern = do
  patterns <- manyTill digit (lexeme (char '|'))
  output <- manyTill digit eol
  pure (patterns, output)

parser :: Parser [SignalPattern]
parser = manyTill signalPattern eof

part1 :: Parsed [SignalPattern] -> IO ()
part1 input = do
  let answer = length
             . filter ((`elem` [2,3,4,7]))
             . concat
             . map (map S.size . snd)
           <$> input
  printAnswer "Times 1, 4, 7 or 8 appear: " answer

decodeOutput (patterns, output) = toInt (map decode output)
  where
    (Just one, Just four, Just seven, Just eight) =
      foldr (\p (mOne, mFour, mSeven, mEight) ->
              case S.size p of
                2 -> (Just p, mFour , mSeven, mEight)
                4 -> (mOne  , Just p, mSeven, mEight)
                3 -> (mOne  , mFour , Just p, mEight)
                7 -> (mOne  , mFour , mSeven, Just p)
                _ -> (mOne  , mFour , mSeven, mEight)
            )
            (Nothing, Nothing, Nothing, Nothing)
            patterns

    (Just zero, Just two, Just three, Just five, Just six, Just nine) =
      foldr (\p (mZero, mTwo, mThree, mFive, mSix, mNine) ->
              case ( S.size (p S.\\ four)
                   , S.size (four S.\\ p)
                   , S.size (p S.\\ seven)
                   , S.size (seven S.\\ p)
                   ) of
                (3,1,3,0) -> (Just p, mTwo  , mThree, mFive , mSix  , mNine )
                (3,2,3,1) -> (mZero , Just p, mThree, mFive , mSix  , mNine )
                (2,1,2,0) -> (mZero , mTwo  , Just p, mFive , mSix  , mNine )
                (2,1,3,1) -> (mZero , mTwo  , mThree, Just p, mSix  , mNine )
                (3,1,4,1) -> (mZero , mTwo  , mThree, mFive , Just p, mNine )
                (2,0,3,0) -> (mZero , mTwo  , mThree, mFive , mSix  , Just p)
                _         -> (mZero , mTwo  , mThree, mFive , mSix  , mNine )
            )
            (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
            patterns

    decode d | d == zero  = 0
             | d == one   = 1
             | d == two   = 2
             | d == three = 3
             | d == four  = 4
             | d == five  = 5
             | d == six   = 6
             | d == seven = 7
             | d == eight = 8
             | d == nine  = 9

    toInt :: [Int] -> Int
    toInt digits = foldr (\d next v -> next (10*v + d)) id digits 0

part2 :: Parsed [SignalPattern] -> IO ()
part2 input = do
  let answer = sum . map decodeOutput <$> input
  printAnswer "Sum of output values: " answer

main :: IO ()
main = do
  let day = "Day 08: Seven Segment Search"
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
