module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.IntMap as M
import Data.List (splitAt, transpose)

import AoC

type Crate = Char
type Crates = M.IntMap String
type Instruction = (Int, Int, Int)
type Input = (Crates, [Instruction])

parseCrate :: Parser Crate
parseCrate = (between (char '[') (char ']') upperChar)
         <|> (string "   " *> pure ' ')

parseIndexes :: Parser [Int]
parseIndexes = hspace *> sepEndBy integer hspace

parseCrates :: Parser Crates
parseCrates = do
  rows <- many (sepBy parseCrate (char ' ') <* eol)
  indexes <- parseIndexes
  eol
  let stacks = map (filter (/= ' ')) . transpose $ rows
  pure . M.fromAscList . zip indexes $ stacks

parseInstruction :: Parser Instruction
parseInstruction = do
  string "move"
  hspace
  quantity <- integer
  hspace
  string "from"
  hspace
  from <- integer
  hspace
  string "to"
  hspace
  to <- integer
  pure (quantity, from, to)

parser :: Parser Input
parser = do
  crates <- parseCrates
  eol
  instructions <- many (parseInstruction <* eol)
  eof
  pure (crates, instructions)

oneByOne :: [Crate] -> [Crate] -> [Crate]
oneByOne = (<>) . reverse

topCrates :: ([Crate] -> [Crate] -> [Crate]) -> Input -> [Crate]
topCrates crateInsertion (crates, instructions)
  = map head
  . M.elems
  $ foldl (\cM (q,f,t) ->
            let topCs = take q (cM M.! f)
             in M.insertWith crateInsertion
                             t
                             topCs
              . M.update (Just . drop q) f
              $ cM
          )
          crates
          instructions

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = topCrates oneByOne <$> input
  printAnswer "Crate on top of each stack: " answer

allAtOnce :: [Crate] -> [Crate] -> [Crate]
allAtOnce = (<>)

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = topCrates allAtOnce <$> input
  printAnswer "Crate on top after correct rearrangement: " answer

main :: IO ()
main = do
  let day = "Day 05: Supply Stacks"
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
