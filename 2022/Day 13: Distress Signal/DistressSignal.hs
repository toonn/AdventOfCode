module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.List (elemIndex, sort)
import Data.Maybe (catMaybes)

import AoC

newtype Packet = Pack { unPack :: Either Int [Packet] } deriving (Eq, Show)

instance Ord Packet where
  compare (Pack (Left l)) (Pack (Left r)) = compare l r
  compare l@(Pack (Left _)) r = compare (Pack (Right [l])) r
  compare l r@(Pack (Left _)) = compare l (Pack (Right [r]))
  compare (Pack (Right ls)) (Pack (Right rs))
    | [] <- ls, [] <- rs = EQ
    | [] <- ls = LT
    | [] <- rs = GT
    | (l:ls') <- ls, (r:rs') <- rs = case compare l r of
                                       EQ -> compare (Pack (Right ls'))
                                                     (Pack (Right rs'))
                                       lgt -> lgt

type Pair = (Packet, Packet)

type Input = [Pair]

packet :: Parser Packet
packet = between (char '[')
                 (char ']')
                 (Pack . Right <$> sepBy ((Pack . Left <$> integer) <|> packet)
                                         (char ',')
                 )

pair :: Parser Pair
pair = do
  a <- packet
  eol
  b <- packet
  pure (a,b)

parser :: Parser Input
parser = sepBy (pair <*eol) eol <* eof

pairs :: Input -> [(Int, Pair)]
pairs = filter ((/= GT) . uncurry compare . snd) . zip [1..]

indices :: [(Int, a)] -> [Int]
indices = map fst

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . indices . pairs <$> input
  printAnswer "Sum of indices of pairs: " answer

dividerPacket :: Int -> Packet
dividerPacket = Pack . Right . (:[]) . Pack . Right . (:[]) . Pack . Left

dividerPackets :: [Packet]
dividerPackets = [dividerPacket 2, dividerPacket 6]

decoderKey :: [Packet] -> Int
decoderKey packets = product
                   . catMaybes
                   . map (\p -> (+1) <$> elemIndex p packets)
                   $ dividerPackets

findDecoderKey :: Input -> Int
findDecoderKey = decoderKey
               . sort
               . (dividerPackets <>)
               . concatMap (\(a,b) -> [a,b])
  where


part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = findDecoderKey <$> input
  printAnswer "Decoder key for the distress signal: " answer

main :: IO ()
main = do
  let day = "Day 13: Distress Signal"
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
