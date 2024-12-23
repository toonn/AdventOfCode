module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Data.Bool (bool)
import Data.List (isPrefixOf)
import qualified Data.Map as M
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
-- inputs :: Int
--        -> Int
--        -> Maybe [(Int, Int)]
--        -> (Int, Int)
--        -> M.Map (Int, [YX]) Int
--        -> ([YX], M.Map (Int, [YX]) Int)
inputs robots layer forbidden (vert,horz) memo
  = let upDown = ((0,1), (1,1))
        leftRight = ((1,0), (1,2))
        vs = replicate (abs vert) (select vert upDown)
        hs = replicate (abs horz) (select horz leftRight)
        sequences = map (<> pure (0,2)) [vs <> hs, hs <> vs]
        safe = filter (forbidden `notPrefixOf`) sequences
     in foldr (\code more (l,c,m) -> let (l',m')
                                           = shortest robots (layer - 1) m code
                                         (lm,c') | null c || l' < l = (l',code)
                                                 | otherwise = (l,c)
                                      in more (lm, c',m')
              )
              (\(_,c,m) -> (c,m))
              safe
              (0, [], memo)

-- shortest :: Int -> Int -> M.Map (Int, [YX]) Int -> [YX]
--          -> (Int, M.Map (Int,[YX]) Int)
shortest robots layer memo code
  | Just l <- memo M.!? (layer,code) = (l,memo)
  | layer == 0 = (length code, M.insert (layer,code) (length code) memo)
  | otherwise
  = let start | layer == robots = (3,2)
              | otherwise = (0,2)
     in foldr (\goal more (from, presses, memo) ->
                let forbidden | fst from == fst start
                              = Just (replicate (snd from) (1,0))
                              | (y,0) <- from = Just
                                              $ if layer == robots
                                                then replicate (3 - y) (1,1)
                                                else replicate y (0,1)
                              | otherwise = Nothing
                    (code', memo')
                      = inputs robots layer forbidden (goal .-. from) memo
                    (l,memo'') = shortest robots (layer - 1) memo' code'
                 in more (goal, presses + l, memo'')
              )
              (\(_, presses, memo) ->
                (presses, M.insert (layer,code) presses memo)
              )
              code
              (start, 0, memo)

complexity :: Int -> [Char] -> Int
complexity robots code
  = fst (shortest robots robots mempty (map keypadCoord code))
  * read (init code)

render :: [YX] -> String
render = map (\(y,x) -> [[ error "The Void!", '^', 'A' ], "<v>" ] !! y !! x)

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . map (complexity 3) <$> input
  printAnswer "Sum of complexities: " answer

-- 123143582477980 too low
-- v<A<A>>^Av<<A>>^AAvAA^<A>Av<A^>AA<Av<A>>^AvA^Av<A<AA>>^AvAA^<A>Av<A^>A<A>Av<A<AA>>^AvAA^<A>Av<A<A>>^AvA^A<A>Av<<A>>^AvA^Av<A<AA>>^AvA^A<Av>A^AAv<<A>>^Av<A>A^A<A>A
-- v<A<AA>>^AvAA^<A>Av<<A>>^AvA^Av<<A>>^Av<A>A^A<A>Av<<A>A^>AA<Av>A^A
-- v<<A>>^A<A>A<Av>A^A<vAA^>A
-- <A^A^>AvvA
-- 026A
part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = sum . map (complexity 26) <$> input
  printAnswer "Sum with 25 robots: " answer

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
