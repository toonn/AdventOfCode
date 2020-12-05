module Main where

import Criterion.Main
import qualified Data.List as L
import Data.Void (Void)
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Paths_AoC2020

type Parser = Parsec Void String
type Parsed a = Either (ParseErrorBundle String Void) a

type SeatID = Integer

binary :: Maybe String -> [Char] -> Parser Integer
binary char chars = do
  cs <- takeWhile1P char (`elem` chars)
  pure $ L.foldl' (\a c -> a * 2 + toNum c) 0 cs

  where
    toNum :: Num a => Char -> a
    toNum c | c == head chars = 0
            | otherwise = 1

row :: Parser Integer
row = binary (Just "Row") "FB"

column :: Parser Integer
column = binary (Just "Column") "LR"

seatID :: Parser SeatID
seatID = do
  r <- row
  c <- column
  pure (r * 8 + c)

seatIDs :: Parser [SeatID]
seatIDs = sepEndBy seatID eol

parseBoardingPasses :: Parser [SeatID]
parseBoardingPasses = seatIDs

readBoardingPasses :: IO (Parsed [SeatID])
readBoardingPasses = do
  inputFile <- getDataFileName "Day 5: Binary Boarding/input.txt"
  parse parseBoardingPasses inputFile
    <$> readFile inputFile

part1 :: Parsed [SeatID] -> IO ()
part1 boardingPasses = do
  let highestSeatID = L.maximum <$> boardingPasses
  either (putStrLn . errorBundlePretty)
         (putStrLn . ("Highest seat ID: " <>) . show)
         highestSeatID

part2 :: Parsed [SeatID] -> IO ()
part2 boardingPasses = do
  let highestSeatID = L.maximum <$> boardingPasses
  either (putStrLn . errorBundlePretty)
         (putStrLn . ("Highest seat ID: " <>) . show)
         highestSeatID

main :: IO ()
main = do
  boardingPasses <- readBoardingPasses
  part1 boardingPasses
  part2 boardingPasses
  defaultMain [
      bgroup "AoC"
        [ bench "Part 1" $ nfIO (silence $ readBoardingPasses >>= part1)
        , bench "Part 2" $ nfIO (silence $ readBoardingPasses >>= part2)
        ]
    ]
