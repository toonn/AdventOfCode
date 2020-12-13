module Main where

import Criterion.Main
import Data.Maybe (fromJust)
import Data.Void (Void)
import System.FilePath ((</>))
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Paths_AoC2020

type Parser = Parsec Void String
type Parsed a = Either (ParseErrorBundle String Void) a

data BusID = X | ID Integer

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space hspace1 empty empty)

integer :: Parser Integer
integer = lexeme L.decimal

bus :: Parser BusID
bus = (char 'x'*> pure X) <|> (ID <$> integer)

timeAndBuses :: Parser (Integer, [BusID])
timeAndBuses = do
  time <- integer
  eol
  buses <- sepBy bus (char ',')
  eol
  eof
  pure $ (time, buses)

readInput :: String -> IO (Parsed (Integer, [BusID]))
readInput day = do
  inputFile <- getDataFileName (day </> "input.txt")
  parse timeAndBuses inputFile
    <$> readFile inputFile

printAnswer :: Show a => String -> Parsed a -> IO ()
printAnswer question answer =
  either (putStrLn . errorBundlePretty)
         (putStrLn . (question <>) . show)
         answer

minWait :: Integer -> BusID -> Maybe (Integer, Integer)
        -> Maybe (Integer, Integer)
minWait _ X b = b
minWait time (ID n) b
  | b == Nothing || wait' < snd (fromJust b) = Just (n, wait')
  | otherwise = b
  where
    wait' = n - (time `rem` n)

productEarliestBusAndTime :: (Integer, [BusID]) -> Integer
productEarliestBusAndTime (time, buses) =
  case foldr (minWait time) Nothing buses of
    Nothing -> 0
    Just (id, wait) -> id * wait

part1 :: Parsed (Integer, [BusID]) -> IO ()
part1 input = do
  let answer = productEarliestBusAndTime <$> input
  printAnswer "Product of earliest bus ID and waiting time: " answer

tagOrDrop :: BusID -> (Integer -> [(Integer, Integer)]) -> Integer
          -> [(Integer, Integer)]
tagOrDrop X rest i = rest (i + 1)
tagOrDrop (ID a) rest i = (a, i) : rest (i + 1)

tag :: [BusID] -> [(Integer, Integer)]
tag buses = foldr tagOrDrop (\_ -> []) buses 0

offset :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
       -> Integer -> Integer
offset a1 a b o1 o a' i | o < 0 = offset a1 a b o1 ((a + o) `rem` b) a' (i - 1)
                        | a'' == o = (i + 1) * a1 + o1
                        | otherwise = offset a1 a b o1 o a'' (i + 1)
  where
    a'' = (a + a') `rem` b

combine' :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
combine' (a, o1) (b, o2) =
  (lcm a b, offset a (a `rem` b) b o1 (-(o1 + o2) `rem` b) 0 0)

combine :: [(Integer, Integer)] -> (Integer, Integer)
combine [x] = x
combine (x:y:rest) = combine (combine' x y : rest)

part2 :: Parsed (Integer, [BusID]) -> IO ()
part2 input = do
  let answer = snd . combine . tag . snd
           <$> input
  printAnswer "Earliest time buses depart at their offsets: " answer

main :: IO ()
main = do
  let day = "Day 13: Shuttle Search"
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
