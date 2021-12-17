module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec hiding (chunk)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Char (digitToInt)

import AoC

type Bits = String

data Packet = Literal Int Int
            | Operator Int Int [Packet]
            deriving Show

hexToBits '0' = "0000"
hexToBits '1' = "0001"
hexToBits '2' = "0010"
hexToBits '3' = "0011"
hexToBits '4' = "0100"
hexToBits '5' = "0101"
hexToBits '6' = "0110"
hexToBits '7' = "0111"
hexToBits '8' = "1000"
hexToBits '9' = "1001"
hexToBits 'A' = "1010"
hexToBits 'B' = "1011"
hexToBits 'C' = "1100"
hexToBits 'D' = "1101"
hexToBits 'E' = "1110"
hexToBits 'F' = "1111"

parser :: Parser Bits
parser = concatMap hexToBits <$> manyTill hexDigitChar (eol *> eof)

bitsToInt :: Bits -> Int
bitsToInt bits = foldr (\b next v -> next (2 * v + digitToInt b))
                       id
                       bits
                       0

bit :: Int -> Parser Bits
bit n = count n (oneOf "01")

number :: Int -> Parser Int
number n = bitsToInt <$> bit n

version :: Parser Int
version = number 3

typeID :: Parser Int
typeID = number 3

literal :: Parser Int
literal = bitsToInt <$> groups
  where
    groups :: Parser Bits
    groups = do
      (prefix:group) <- bit 5
      groups <- case prefix of
        '0' -> pure []
        '1' -> groups
      pure (group <> groups)

packetBits :: Int -> Parser [Packet]
packetBits 0 = pure []
packetBits n = do
  (bits, p) <- match packet
  ps <- packetBits (n - length bits)
  pure (p:ps)

packet :: Parser Packet
packet = do
  v <- version
  tID <- typeID
  case tID of
    4 -> Literal v <$> literal
    _ -> do
      lTID <- number 1
      ps <- case lTID of
        0 -> do
          totalBits <- number 15
          packetBits totalBits
        1 -> do
          totalPackets <- number 11
          count totalPackets packet
      pure (Operator v tID ps)

collectVersions :: Packet -> [Int]
collectVersions (Literal v _)     = [v]
collectVersions (Operator v _ ps) = v : concatMap collectVersions ps

part1 :: Parsed Bits -> IO ()
part1 input = do
  let answer = sum . collectVersions <$> (parse packet "Bits" =<< input)
  printAnswer "Sum of packet versions: " answer

operator :: Int -> ([Int] -> Int)
operator 0 = sum
operator 1 = product
operator 2 = minimum
operator 3 = maximum
operator 5 = \[p1,p2] -> if p1 > p2 then 1 else 0
operator 6 = \[p1,p2] -> if p1 < p2 then 1 else 0
operator 7 = \[p1,p2] -> if p1 == p2 then 1 else 0

evaluate :: Packet -> Int
evaluate (Literal _ v) = v
evaluate (Operator _ tID ps) = operator tID (map evaluate ps)

part2 :: Parsed Bits -> IO ()
part2 input = do
  let answer = evaluate <$> (parse packet "Bits" =<< input)
  printAnswer "Expression value: " answer

main :: IO ()
main = do
  let day = "Day 16: Packet Decoder"
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
