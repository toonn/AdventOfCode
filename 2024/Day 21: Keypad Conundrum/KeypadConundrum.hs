module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Data.Tuple (swap)
import Data.Bool (bool)
import Data.List (isPrefixOf, minimumBy)
import Data.Maybe (maybe)

type Input = [[Char]]

parser :: Parser Input
parser = characterGrid <* eof

getIndex :: Char -> [[Char]] -> Int
getIndex c = fst . (\(h:_) -> h) . filter snd . zip [0..] . map (c `elem`)

keypadCoord :: Char -> YX
keypadCoord key = ( getIndex key [ "789", "456", "123", "0A" ]
                  , getIndex key [ "741", "8520", "963A" ]
                  )

(.-.) :: Num a => (a,a) -> (a,a) -> (a,a)
(a1,b1) .-. (a2,b2) = (a1 - a2, b1 - b2)

select :: (Num i, Ord i) => i -> (a,a) -> a
select = flip (uncurry bool) . (> 0)

notPrefixOf :: Eq a => Maybe [a] -> [a] -> Bool
notPrefixOf = (not .) . (. flip isPrefixOf) . flip (maybe False)

inputs :: Int -> Maybe [(Int, Int)] -> (Int, Int) -> [YX]
inputs layer forbidden (vert,horz)
  = let upDown = ((0,1), (1,1))
        leftRight = ((1,0), (1,2))
        vs = replicate (abs vert) (select vert upDown)
        hs = replicate (abs horz) (select horz leftRight)
        sequences = map (<> pure (0,2)) [vs <> hs, hs <> vs]
        safe = filter (forbidden `notPrefixOf`) sequences
        compareOnShortest = curry ( uncurry compare
                                  . both (shortest (layer - 1))
                                  )
     in minimumBy compareOnShortest safe

shortest :: Int -> [YX] -> Int
shortest layer code
  | layer == 0 = length code
  | otherwise
  = let start | layer == 3 = (3,2)
              | otherwise = (0,2)
     in foldr (\goal more from ->
                let forbidden | fst from == fst start
                              = Just (replicate (snd from) (1,0))
                              | (y,0) <- from = Just
                                              $ if layer == 3
                                                then replicate (3 - y) (1,1)
                                                else replicate y (0,1)
                              | otherwise = Nothing
                    code' = inputs layer forbidden (goal .-. from)
                 in shortest (layer - 1) code' + more goal
              )
              (const 0)
              code
              start

-- 379A
-- ^A^^<<A>>AvvvA
-- <A>A<AAv<AA>>^AvAA^Av<AAA>^A
-- v<<A>>^AvA^Av<<A>>^AAv<A<A>>^AAvAA<^A>Av<A>^AA<A>Av<A<A>>^AAAvA^<A>A
--
-- <v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A
-- <A>Av<<AA>^AA>AvAA^A<vAAA>^A
-- ^A<<^^A>>AvvvA
-- 379A
--
-- difference
-- v<<A>>^AAv<A<A>>^AAvAA<^A
-- <AAv<AA>>^
-- v<<AA>^AA>
-- <vA<AA>>^AAvA<^A>AAvA
complexity :: [Char] -> Int
complexity code = shortest 3 (map keypadCoord code) * read (init code)

render :: [YX] -> String
render = map (\(y,x) -> [[ error "The Void!", '^', 'A' ], "<v>" ] !! y !! x)

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . map complexity <$> input
  printAnswer "Sum of complexities: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = Right 'P' -- const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 21: Keypad Conundrum"
  input <- readInput day parser
  putStrLn ""
  part1 input
  part2 input
  putStrLn ""
  defaultMainWith (defaultConfig { resamples = 1 }) [
      bgroup "AoC"
        [ bench "Part 1" $ nfIO (silence $ readInput day parser >>= part1)
        , bench "Part 2" $ nfIO (silence $ readInput day parser >>= part2)
        ]
    ]
