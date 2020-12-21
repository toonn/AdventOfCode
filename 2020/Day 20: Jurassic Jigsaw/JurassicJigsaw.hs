module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AoC

import qualified Data.Map as M
import qualified Data.Monoid as M
import qualified Data.Set as S

data Tile = Tile Integer [[Char]]
  deriving Show
data Border = Border Integer [[Char]]
  deriving Show

tile :: Parser Tile
tile = do
  lexeme (string "Tile")
  iD <- integer
  lexeme (char ':')
  eol
  pixels <- sepEndBy (takeWhile1P (Just "Pixel") (`elem` "#.")) eol
  pure $ Tile iD pixels

tiles :: Parser [Tile]
tiles = sepEndBy tile eol <* eof

multiply :: Num a => [a] -> a
multiply = M.getProduct . mconcat . map M.Product

cornerIDs :: M.Map [Char] [Integer] -> [Integer]
cornerIDs m = M.foldr ( \(iD:rest) next seen ->
                          if null rest
                          then if S.member iD seen
                               then iD:next seen
                               else next (S.insert iD seen)
                          else next seen
                      )
                      (const [])
                      m
                      S.empty

canonicalize :: Ord a => [a] -> [a]
canonicalize as = let as' = reverse as in case compare as as' of
  LT -> as
  EQ -> as
  GT -> as'

tally :: M.Map [Char] [Integer] -> [Border] -> M.Map [Char] [Integer]
tally m [] = m
tally m (Border iD edges:bs) =
  tally ( foldr (\e m' -> M.insertWith (\[x] xs -> x:xs) e [iD] m')
                m
                (map canonicalize edges)
        )
        bs

borders :: Tile -> Border
borders (Tile iD tile) = Border iD [top, right, bottom, left]
  where
    top = head tile
    right = map last tile
    bottom = reverse (last tile)
    left = reverse (map head tile)

part1 :: Parsed [Tile] -> IO ()
part1 input = do
  let answer = multiply . cornerIDs . tally M.empty . map borders <$> input
  printAnswer "Product of corner tile IDs: " answer

part2 :: Parsed [Tile] -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "Not an answer: " answer

main :: IO ()
main = do
  let day = "Day 20: Jurassic Jigsaw"
  let parser = tiles
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
