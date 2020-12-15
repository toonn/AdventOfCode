module Main where

import Criterion.Main
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Void (Void)
import System.FilePath ((</>))
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Paths_AoC2020

type Parser = Parsec Void String
type Parsed a = Either (ParseErrorBundle String Void) a

type Numbers i m = (i, i, m i)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space hspace1 empty empty)

integer :: Parser Int
integer = lexeme L.decimal

numbers :: (Num i, Ord i, Enum i) => Parser (Numbers i (M.Map i))
numbers = do
  ns <- sepBy integer (char ',') <* eol <* eof
  let seen = foldr (\(i,n) m -> M.insert (fromIntegral n) i m)
                   M.empty
                   (zip [1..] (init ns))
  pure (fromIntegral (length ns), fromIntegral (last ns), seen)

readInput :: (Num i, Ord i, Enum i) => String
          -> IO (Parsed (Numbers i (M.Map i)))
readInput day = do
  inputFile <- getDataFileName (day </> "input.txt")
  parse numbers inputFile
    <$> readFile inputFile

printAnswer :: Show a => String -> Parsed a -> IO ()
printAnswer question answer =
  either (putStrLn . errorBundlePretty)
         (putStrLn . (question <>) . show)
         answer

th :: Integer -> [Numbers Integer (M.Map Integer)] -> Integer
th i ((j, n, _):ns) | i == j = n
                    | i < j = error "Can't index starting numbers"
                    | otherwise = th i ns

age :: Num i => (m i -> i -> Maybe i) -> Numbers i m -> i
age index (i, n, seen) = case seen `index` n of
  Nothing -> 0
  Just j -> i - j

recite :: Numbers Integer (M.Map Integer) -> Numbers Integer (M.Map Integer)
recite (i, n, seen) = (i+1, age (M.!?) (i, n, seen), M.insert n i seen)

part1 :: Parsed (Numbers Integer (M.Map Integer)) -> IO ()
part1 input = do
  let answer = (2020 `th`) . iterate recite <$> input
  printAnswer "The 2020th number is: " answer

reciteFastTo :: Int -> Numbers Int IM.IntMap -> Int
reciteFastTo i (j, n, seen)
  | i == j = n
  | otherwise = reciteFastTo i (j + 1, age (IM.!?) (j, n, seen), IM.insert n j seen)

toIntMap :: Numbers Int (M.Map Int) -> Numbers Int IM.IntMap
toIntMap (i, n, seen) = (i, n, IM.fromDistinctAscList (M.toAscList seen))

part2 :: Parsed (Numbers Int (M.Map Int)) -> IO ()
part2 input = do
  let answer = reciteFastTo 30000000
             -- A Mutable vector should be faster than a Map.
             . toIntMap
           <$> input
  printAnswer "The 30000000th number is: " answer

main :: IO ()
main = do
  let day = "Day 15: Rambunctious Recitation"
  input <- readInput day
  input2 <- readInput day
  putStrLn ""
  part1 input
  part2 input2
  putStrLn ""
  defaultMain [
      bgroup "AoC"
        [ bench "Part 1" $ nfIO (silence $ readInput day >>= part1)
        , bench "Part 2" $ nfIO (silence $ readInput day >>= part2)
        ]
    ]
