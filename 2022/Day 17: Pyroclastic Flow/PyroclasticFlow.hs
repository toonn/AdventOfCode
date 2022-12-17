module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.List (intercalate)
import qualified Data.Set as S

import AoC

type Coord = (Int, Int)
type Rock = S.Set Coord
type JetPattern = String

newtype RockString = RS { unRS :: String }

instance Show RockString where
  show = unRS

type Input = JetPattern

parser :: Parser Input
parser = takeWhileP (Just "Jet direction") (`elem` "<>") <* eol <* eof

rocks :: [Coord -> Rock]
rocks = map ((\s (x,y) -> S.map (\(dx,dy) -> (x+dx,y+dy)) s) . S.fromAscList)
            [ [(dx,0) | dx <- [0..3]] -- ^ "-" shape
            , [(0,1)]
           <> [(1,dy) | dy <- [0..2]]
           <> [(2,1)]                 -- ^ "+" shape
            , [(dx,0) | dx <- [0..2]]
           <> [(2,dy) | dy <- [1..2]] -- ^ reverse "L" shape
            , [(0,dy) | dy <- [0..3]] -- ^ "|" shape
            , [(dx,dy) | dx <- [0..1]
                       , dy <- [0..1]
              ]                       -- ^ square shape
            ]

push :: Char -> Int -> Int
push jet x | jet == '<' = max 0 (x - 1)
           | jet == '>' = min (7-1) (x + 1)

inBounds :: Rock -> Bool
inBounds cs | let xs = S.map fst cs
            = 0 <= S.findMin xs && S.findMax xs < 7

dropRocks :: [Coord -> Rock] -> JetPattern -> Rock -> Rock
dropRocks [] _ tower = tower
dropRocks (rock:rocks) jetPattern tower
  = uncurry (dropRocks rocks)
  $ (foldr (\y more x (j:jP) ->
             let pushedX = push j x
                 pushedRock = rock (pushedX,y)
                 x' | S.disjoint tower pushedRock
                    , inBounds pushedRock
                    = pushedX
                    | otherwise = x
                 droppedY = y - 1
                 droppedRock = rock (x', droppedY)
                 (jP',tower') | not (S.disjoint tower droppedRock)
                              = (jP, rock (x', y) `S.union` tower)
                              | otherwise = more x' jP
              in (jP', tower')
           )
           (\x (j:jP) -> let pushedX = push j x
                             pushedRock = rock (pushedX,0)
                             rock' | inBounds pushedRock = pushedRock
                                   | otherwise = rock (x,0)

                          in (jP, rock' `S.union` tower)
           )
           ([startY,startY-1..1])
           2
           jetPattern
    )
  where
    startY = 3 + height tower

fall :: Int -> Input -> Rock
fall nrRocks jetPattern = dropRocks (take nrRocks (cycle rocks))
                                    (cycle jetPattern)
                                    S.empty

height :: Rock -> Int
height tower = case S.lookupMax (S.map snd tower) of
                 Nothing -> 0
                 Just y -> y + 1

render :: Rock -> RockString
render tower = RS . ('\n':) . intercalate "\n"
             $ [map (\x -> token (x,y)) [0..7-1] | y <- [maxY, maxY-1..0]]
  where
    maxY = S.findMax . S.map snd $ tower
    token c | c `S.member` tower = '#'
            | otherwise = '.'

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = height . fall 2022 <$> input
  printAnswer "Tower height after 2022 rocks have fallen: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const "P" <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 17: Pyroclastic Flow"
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
