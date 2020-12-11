module Main where

import Criterion.Main
import qualified Data.Vector as V
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
type Seats = V.Vector (V.Vector Seat)

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

nrOccupied :: [Seat ]-> Int
nrOccupied = length . filter (== Occupied)

rule1Row :: [Seat] -> [Seat] -> [Seat] -> [Seat]
rule1Row a b c = Empty : go a b c
  where
    go (ul:above@(um:ur:_)) (cl:current@(cc:cr:_)) (bl:below@(bm:br:_))
      | cc == Empty   , nrOccupied adjacentSeats == 0 = Occupied : rest
      | cc == Occupied, nrOccupied adjacentSeats >= 4 = Empty : rest
      | otherwise                                     = cc : rest
      where
        adjacentSeats = [ul, um, ur, cl, cr, bl, bm, br]
        rest = go above current below
    go _ (_:Floor:_) _ = [Floor]

rule1 :: [[Seat]] -> [[Seat]]
rule1 seats = emptyRow :
  zipWith3 rule1Row seats (drop 1 seats) (drop 2 seats)
  <> [emptyRow]
  where
    emptyRow = replicate (length (head seats)) Floor

simulate :: Eq a => (a -> a) -> a -> a
simulate step seats | seats == seats' = seats'
                    | otherwise = simulate step seats'
  where
    seats' = step seats

part1 :: Parsed [[Seat]] -> IO ()
part1 input = do
  let answer = nrOccupied . concat . simulate rule1 <$> input
  printAnswer "Occupied seats: " answer

rule2Seat :: Seats -> Int -> Int -> Seat -> Seat
rule2Seat seats y x seat
  | seat == Empty, nrOccupied visibleSeats == 0 = Occupied
  | seat == Occupied, nrOccupied visibleSeats >= 5 = Empty
  | otherwise = seat
  where
    directions = [(dx, dy) | dx <- [-1,0,1], dy <- [-1,0,1], dx /= 0 || dy /= 0]
    -- Precomputing firstVisible for the whole step could be more efficient.
    -- Someone mentioned you only need to check seats that either changed in
    -- the last step or which have an adjacent or visible which changed.
    firstVisible (x,y) (dx, dy)
      | x' == 0 || y' == 0
        || y' == V.length seats - 1 || x' == V.length (seats V.! y) - 1
        = Empty
      | seat' == Floor = firstVisible (x',y') (dx,dy)
      | otherwise = seat'
      where
        x' = x + dx
        y' = y + dy
        seat' = seats V.! y' V.! x'
    visibleSeats = map (firstVisible (x,y)) directions

rule2Row :: Seats -> Int -> V.Vector Seat -> V.Vector Seat
rule2Row seats y row | y == 0 || y == V.length seats - 1 = row
                     | otherwise = V.imap (rule2Seat seats y) row

rule2 :: Seats -> Seats
rule2 seats = V.imap (rule2Row seats) seats

part2 :: Parsed [[Seat]] -> IO ()
part2 input = do
  let answer = nrOccupied . concat
             . map V.toList . V.toList
             . simulate rule2
             . V.fromList . map V.fromList
           <$> input
  printAnswer "Occupied seats by sight: " answer

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
