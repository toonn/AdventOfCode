module Main where

import Criterion.Main
import qualified Data.List as L
import Data.Void (Void)
import System.FilePath ((</>))
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Paths_AoC2020

type Parser = Parsec Void String
type Parsed a = Either (ParseErrorBundle String Void) a

data Seat = Empty | Occupied | Floor
  deriving Eq

seat :: Char -> Seat
seat 'L' = Empty
seat '.' = Floor

row :: Parser [Seat]
row = map seat . ('.':) . (<> ".") <$> takeWhile1P (Just "Seat") (`elem` "L.")

seats :: Parser [[Seat]]
seats = do
  rows <- sepEndBy row eol
  let emptyRow = replicate (length (head rows)) Floor
  pure $ emptyRow : rows <> [emptyRow]

readInput :: String -> IO (Parsed [[Seat]])
readInput day = do
  inputFile <- getDataFileName (day </> "input.txt")
  parse seats inputFile
    <$> readFile inputFile

printAnswer :: Show a => String -> Parsed a -> IO ()
printAnswer question answer =
  either (putStrLn . errorBundlePretty)
         (putStrLn . (question <>) . show)
         answer

nrOccupied :: [Seat] -> Int
nrOccupied = length  . filter (== Occupied)

stepRow :: [Seat] -> [Seat] -> [Seat] -> [Seat]
stepRow a b c = Empty : go a b c
  where
    go (ul:above@(um:ur:_)) (cl:current@(cc:cr:_)) (bl:below@(bm:br:_))
      | cc == Empty   , nrOccupied adjacentSeats == 0 = Occupied : rest
      | cc == Occupied, nrOccupied adjacentSeats >= 4 = Empty : rest
      | otherwise                                     = cc : rest
      where
        adjacentSeats = [ul, um, ur, cl, cr, bl, bm, br]
        rest = go above current below
    go _ (_:Floor:_) _ = [Floor]

step :: [[Seat]] -> [[Seat]]
step seats = emptyRow :
  zipWith3 stepRow seats (drop 1 seats) (drop 2 seats)
  <> [emptyRow]
  where
    emptyRow = replicate (length (head seats)) Floor

simulate :: [[Seat]] -> [[Seat]]
simulate seats | seats == seats' = seats'
               | otherwise = simulate seats'
  where
    seats' = step seats

part1 :: Parsed [[Seat]] -> IO ()
part1 input = do
  let answer = nrOccupied . concat . simulate <$> input
  printAnswer "Occupied seats: " answer

part2 :: Parsed [[Seat]] -> IO ()
part2 input = do
  let answer = const "P" <$> input
  printAnswer "Not an answer: " answer

main :: IO ()
main = do
  let day =  "Day 11: Seating System"
  input <- readInput day
  putStrLn ""
  part1 input
  part2 input
  putStrLn ""
  defaultMain [
      bgroup "AoC"
        [ bench "Part 1" $ nfIO (silence $ readInput day >>= part1)
        , bench "Part 2" $ nfIO (silence $ readInput day >>= part2)
        ]
    ]
