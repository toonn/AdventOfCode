{-# LANGUAGE RankNTypes #-}
module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AoC

import qualified Data.List as L
import qualified Data.Set as S

data Direction = E | W | SE | NW | SW | NE
  deriving (Eq, Ord, Show)
type Instruction = [Direction]
type CInstruction = (Int, Int)

instruction :: Parser Instruction
instruction = do
  directions <- takeWhile1P (Just "Direction") (`elem` "eswn")
  pure . map ( \d -> case d of
                ('e',_) -> E
                ('w',_) -> W
                ('s','e') -> SE
                ('s','w') -> SW
                ('n','e') -> NE
                ('n','w') -> NW
             )
       $ zip directions (tail directions)

instructions :: Parser [Instruction]
instructions = endBy instruction eol <* eof

canonicalize :: Instruction -> CInstruction
canonicalize instruction = foldr (\d next (e,se) ->
                                    case d of
                                      E  -> next (e+1,se)
                                      W  -> next (e-1,se)
                                      SE -> next (e,se+1)
                                      NW -> next (e,se-1)
                                      SW -> next (e-1,se+1)
                                      NE -> next (e+1,se-1)
                                 )
                                 id
                                 instruction
                                 (0,0)

flipped :: [CInstruction] -> S.Set CInstruction
flipped = foldr ( \inst seen -> if S.member inst seen
                                then S.delete inst seen
                                else S.insert inst seen
                )
                S.empty

part1 :: Parsed [Instruction] -> IO ()
part1 input = do
  let answer = S.size . flipped . map canonicalize <$> input
  printAnswer "Nr of tiles with black side up: " answer

part2 :: Parsed [Instruction] -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "Not an answer: " answer

main :: IO ()
main = do
  let day = "Day 24: Lobby Layout"
  let parser = instructions
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
