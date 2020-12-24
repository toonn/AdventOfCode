{-# LANGUAGE RankNTypes #-}
module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AoC

import Control.Monad (guard)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

data Direction = E | W | SE | NW | SW | NE
  deriving (Eq, Ord, Show)
type Instruction = [Direction]
type CInstruction = (Int, Int)

toDirections :: String -> Instruction
toDirections [] = []
toDirections ('e':rest) = E : toDirections rest
toDirections ('w':rest) = W : toDirections rest
toDirections ('s':'e':rest) = SE : toDirections rest
toDirections ('n':'w':rest) = NW : toDirections rest
toDirections ('s':'w':rest) = SW : toDirections rest
toDirections ('n':'e':rest) = NE : toDirections rest

instruction :: Parser Instruction
instruction = toDirections <$> takeWhile1P (Just "Direction") (`elem` "eswn")
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

adjacent :: CInstruction -> [CInstruction]
adjacent (x,y) = do
  i <- [-1,0,1]
  j <- [-1,0,1]
  guard (i /= j)
  pure (x+i,y+j)

flipTiles :: Int -> S.Set CInstruction -> S.Set CInstruction
flipTiles 0 tiles = tiles
flipTiles times tiles = flipTiles (times - 1) (M.keysSet black)
  where
    neighbors = S.foldr ( \t ns ->
                           foldr (\n -> M.insertWith (+) n 1) ns (adjacent t)
                        )
                        M.empty
                        tiles
    black = M.filterWithKey ( \t n -> case S.member t tiles of
                                        True  | n == 0 || n > 2 -> False
                                              | otherwise -> True
                                        False | n == 2 -> True
                                              | otherwise -> False
                            )
                            neighbors

part2 :: Parsed [Instruction] -> IO ()
part2 input = do
  let answer = S.size . flipTiles 100 . flipped . map canonicalize <$> input
  printAnswer "Nr of black tiles after 100 days: " answer

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
