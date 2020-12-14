module Main where

import Criterion.Main
import Data.Bits
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

type Address = Integer
type Value = Integer
type Mask = String
data Instruction = Mask Mask | Mem Address Value
type Instructions = [Instruction]
type Memory a = M.Map Address a

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space hspace1 empty empty)

integer :: Parser Integer
integer = lexeme L.decimal

word :: String -> Parser String
word s = lexeme (string s)

equals :: Parser Char
equals = lexeme (char '=')

bits :: Parser [Char]
bits = lexeme (takeWhile1P (Just "Mask bit") (`elem` "X01"))

mask :: Parser Instruction
mask = do
  word "mask"
  equals
  value <- bits
  pure $ Mask value

mem :: Parser Instruction
mem = do
  word "mem"
  address <- between (char '[') (char ']') integer
  hspace
  equals
  value <- integer
  pure $ Mem address value

instruction :: Parser Instruction
instruction = choice
  [ mask
  , mem
  ]

instructions :: Parser Instructions
instructions = sepEndBy instruction eol <* eof

readInput :: String -> IO (Parsed Instructions)
readInput day = do
  inputFile <- getDataFileName (day </> "input.txt")
  parse instructions inputFile
    <$> readFile inputFile

printAnswer :: Show a => String -> Parsed a -> IO ()
printAnswer question answer =
  either (putStrLn . errorBundlePretty)
         (putStrLn . (question <>) . show)
         answer

zeros :: Integer -> [Char] -> Integer
zeros x [] = x
zeros x ('0':rest) = zeros (2*x) rest
zeros x (_:rest) = zeros (2*x + 1) rest

ones :: Integer -> [Char] -> Integer
ones x [] = x
ones x ('1':rest) = ones (2*x + 1) rest
ones x (_:rest) = ones (2*x) rest

maskToPair :: Mask -> (Value, Value)
maskToPair value = (zeros 0 value, ones 0 value)

applyMask :: Value -> Mask -> Value
applyMask val mask = val .&. zeros .|. ones
  where
    (zeros, ones) = maskToPair mask

initialize :: Mask -> Memory (Value, Mask)-> Instructions
           -> Memory (Value, Mask)
initialize _ memory [] = memory
initialize mask memory (Mask v:rest) = initialize v memory rest
initialize mask memory (Mem a v:rest) =
  initialize mask (M.insert a (v, mask) memory) rest

sumMemory :: Memory (Value, Mask) -> Integer
sumMemory = M.foldr (\(v, mask) x -> x + applyMask v mask) 0

part1 :: Parsed Instructions -> IO ()
part1 input = do
  let answer = sumMemory
             . initialize "" M.empty
           <$> input
  printAnswer "Sum of values in memory: " answer

combinations :: Num a => [a] -> [a] -> [a]
combinations as [] = as
combinations as bs = do
  a <- as
  b <- bs
  pure (a + b)

addresses :: Address -> Mask -> [Address]
addresses a m = foldr combinations [] $ do
  (power, mbit) <- zip [35,34..0] m
  let factor = case mbit of
        '0' | testBit a power -> [2^power]
            | otherwise -> [0]
        '1' -> [2^power]
        'X' -> [0, 2^power]
  pure factor

initialize2 :: Mask -> Memory Value -> Instructions -> Memory Value
initialize2 _ memory [] = memory
initialize2 mask memory (Mask m:rest) = initialize2 m memory rest
initialize2 mask memory (Mem a v:rest) = initialize2 mask memory' rest
  where
    memory' = foldr (\a' m -> M.insert a' v m) memory (addresses a mask)

sumValues :: Memory Value -> Integer
sumValues = M.foldr (+) 0

part2 :: Parsed Instructions -> IO ()
part2 input = do
  let answer = sumValues
             . initialize2 "" M.empty
           <$> input
  printAnswer "Sum of values in memory with version 2 decoder: " answer

main :: IO ()
main = do
  let day = "Day 14: Docking Data"
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
