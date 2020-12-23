module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AoC

import Data.Char (digitToInt, intToDigit, isDigit)
import Data.Foldable (toList)
import qualified Data.Map as M
import qualified Data.Sequence as Seq

type Cups = Seq.Seq Integer

cups :: Parser Cups
cups = Seq.fromList . map (fromIntegral . digitToInt)
   <$> (takeWhile1P (Just "Digit") isDigit <* eol <* eof)

destination :: Integer -> Integer -> Cups -> Integer
destination max 0 three = destination max max three
destination max cup three = case Seq.elemIndexL cup three of
  Nothing -> cup
  Just _ -> destination max (cup - 1) three

pop :: Ord k => k -> M.Map k v -> (Maybe v, M.Map k v)
pop = M.updateLookupWithKey (\_ _ -> Nothing)

insert :: (M.Map Integer Cups, Cups) -> (M.Map Integer Cups, Cups)
insert (inserts, (cup Seq.:<| cups)) = case insert (inserts', cups') of
  (inserts'', cups'') -> (inserts'', cup Seq.:<| cups'')
  where
    (mCs, inserts') = pop cup inserts
    cups' | Just cs <- mCs = cs <> cups
          | otherwise = cups
insert cups = cups

play :: Integer -> Integer -> M.Map Integer Cups -> Cups -> Cups
play _ 0 inserts cups = snd (insert (inserts, cups))
play max moves inserts (current Seq.:<| cups) =
  let
      (inserts', c1 Seq.:<| cups')
        | (Just cs, inserts') <- pop current inserts
          = (inserts', cs <> cups)
        | otherwise = (inserts, cups)
      (inserts'', c2 Seq.:<| cups'')
        | (Just cs, inserts'') <- pop c1 inserts'
          = (inserts'', cs <> cups')
        | otherwise = (inserts', cups')
      (inserts''', c3 Seq.:<| cups''')
        | (Just cs, inserts''') <- pop c2 inserts''
          = (inserts''', cs <> cups'')
        | otherwise = (inserts'', cups'')
      three = Seq.fromList [c1,c2,c3]
      dest = destination max (current - 1) three
      (inserts'''', three') = insert (inserts''', three)
      (three'', rest) = Seq.splitAt 3 three'
      inserts''''' = M.insertWith (<>) dest three'' inserts''''
      cups'''' = rest <> cups'''
   in play max (moves - 1) inserts''''' (cups'''' Seq.:|> current)

labels :: Cups -> String
labels cups | Just index <- Seq.elemIndexL 1 cups =
  case Seq.splitAt index cups of
    (before, _ Seq.:<| after) ->
      map (intToDigit . fromIntegral) (toList (after <> before))

part1 :: Parsed Cups -> IO ()
part1 input = do
  let answer = labels . (\cups -> play (maximum cups) 100 M.empty cups)
           <$> input
  printAnswer "Labels after cup 1: " answer

productTwo :: Cups -> Integer
productTwo cups | Just index <- Seq.elemIndexL 1 cups =
  let (x,y) = case (fromIntegral index + 2) `compare` Seq.length cups of
        LT -> (Seq.index cups (index + 1), Seq.index cups (index + 2))
        EQ -> (Seq.index cups (index + 1), Seq.index cups 0)
        GT -> (Seq.index cups 0, Seq.index cups 1)
   in x * y

part2 :: Parsed Cups -> IO ()
part2 input = do
  let answer = productTwo
             . play 1000000 10000000 M.empty
             . (\cups -> cups <> Seq.fromList [maximum cups + 1..1000000])
           <$> input
  printAnswer "Product of two cups following label 1: " answer

main :: IO ()
main = do
  let day = "Day 23: Crab Cups"
  let parser = cups
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
