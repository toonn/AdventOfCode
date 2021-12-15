module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec hiding (chunk)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Map as M

import AoC

type Polymer = String
type Rules = M.Map Polymer Char

polymer :: Parser Polymer
polymer = lexeme (many letterChar)

rule :: Parser (Polymer, Char)
rule = do
  pair <- polymer
  lexeme (string "->")
  insert <- lexeme letterChar
  eol
  pure (pair, insert)

parser :: Parser (Polymer, Rules)
parser = do
  template <- polymer
  eol
  eol
  rules <- M.fromList <$> manyTill rule eof
  pure (template, rules)

polymerize :: Rules -> Polymer -> Polymer
polymerize rules (e:es) = e:(concatMap (\(c,n) -> case M.lookup [c,n] rules of
                                         Nothing -> [n]
                                         Just i -> [i,n]
                                       )
                                       (zip (e:es) es)
                            )

freqs :: Polymer -> M.Map Char Int
freqs = foldr (\e -> M.insertWith (+) e 1)
              M.empty

part1 :: Parsed (Polymer, Rules) -> IO ()
part1 input = do
  let answer = (\fs -> (maximum fs) - (minimum fs))
             . M.elems
             . freqs
             . (\(template, rules) -> iterate (polymerize rules) template !! 10)
           <$> input
  printAnswer "Difference between most and least common element: " answer

pairs :: Polymer -> M.Map Polymer Int
pairs polymer = foldr (\p -> M.insertWith (+) p 1)
                      M.empty
                      (zipWith (\p1 p2 -> [p1,p2]) polymer (tail polymer))

polymerizePairs :: Rules -> M.Map Polymer Int -> M.Map Polymer Int
polymerizePairs rules = M.foldrWithKey
  (\pair@[p1,p2] count m -> case M.lookup pair rules of
    Nothing -> M.insert pair count m
    Just e  -> M.insertWith (+) [p1,e] count
             . M.insertWith (+) [e,p2] count
             $ m
  )
  M.empty

freqs' :: Char -> M.Map Polymer Int -> M.Map Char Int
freqs' first = M.foldrWithKey (\[_,p2] count m -> M.insertWith (+) p2 count m)
                              (M.singleton first 1)

part2 :: Parsed (Polymer, Rules) -> IO ()
part2 input = do
  let answer = (\fs -> (maximum fs) - (minimum fs))
             . M.elems
             . (\(template, rules) -> freqs' (head template)
                                             (iterate (polymerizePairs rules)
                                                      (pairs template)
                                              !! 40
                                             )
               )
           <$> input
  printAnswer "Difference between most and least common element: " answer

main :: IO ()
main = do
  let day = "Day 14: Extended Polymerization"
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
