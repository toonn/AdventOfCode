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
type Mask = (Value, Value)
data Instruction = Mask Mask | Mem Address Value
type Instructions = [Instruction]
type Memory = M.Map Address (Value, Mask)

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

zeros :: Integer -> [Char] -> Integer
zeros x [] = x
zeros x ('0':rest) = zeros (2*x) rest
zeros x (_:rest) = zeros (2*x + 1) rest

ones :: Integer -> [Char] -> Integer
ones x [] = x
ones x ('1':rest) = ones (2*x + 1) rest
ones x (_:rest) = ones (2*x) rest

mask :: Parser Instruction
mask = do
  word "mask"
  equals
  value <- bits
  pure $ Mask (zeros 0 value, ones 0 value)

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

applyMask :: Value -> Mask -> Value
applyMask val (zeros, ones) = val .&. zeros .|. ones

initialize :: Mask -> Memory -> Instructions -> Memory
initialize _ memory [] = memory
initialize mask memory (Mask v:rest) = initialize v memory rest
initialize mask memory (Mem a v:rest) =
  initialize mask (M.insert a (v, mask) memory) rest

sumMemory :: Memory -> Integer
sumMemory = M.foldr (\(v, mask) x -> x + applyMask v mask) 0

part1 :: Parsed Instructions -> IO ()
part1 input = do
  let answer = sumMemory
             . initialize (2^36-1, 0) M.empty
           <$> input
  printAnswer "Sum of values in memory: " answer

part2 :: Parsed Instructions -> IO ()
part2 input = do
  let answer = const 'P'
           <$> input
  printAnswer "Not an answer: " answer

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
