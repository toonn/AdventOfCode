module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.List (intersperse)
import qualified Data.Map as M
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S

import AoC

type Coord = (Int, Int)
type Directions = [Coord -> S.Set Coord -> Maybe Coord]

type Input = S.Set Coord

row :: Parser [Int]
row = catMaybes
    . zipWith (\x t -> if t == '#' then Just x else Nothing)
              [0..]
  <$> takeWhile1P (Just "Tile") (`elem` ".#")

parser :: Parser Input
parser = do
  rows <- sepEndBy row eol
  eof
  pure (S.fromAscList . concat . zipWith (\y -> map (\x -> (y,x))) [0..] $ rows)

neighbors :: [Int] -> [Int] -> Coord -> S.Set Coord
neighbors yDs xDs (y,x) = S.fromAscList [(y+yd,x+xd) | yd <- yDs
                                                     , xd <- xDs
                                                     , yd /= 0 || xd /= 0
                                        ]

free :: S.Set Coord -> S.Set Coord -> Bool
free ns = S.null . S.intersection ns

northFree :: Coord -> S.Set Coord -> Bool
northFree = free . neighbors [-1] [-1..1]

eastFree :: Coord -> S.Set Coord -> Bool
eastFree = free . neighbors [-1..1] [1]

southFree :: Coord -> S.Set Coord -> Bool
southFree = free . neighbors [1] [-1..1]

westFree :: Coord -> S.Set Coord -> Bool
westFree = free . neighbors [-1..1] [-1]

alone :: Coord -> S.Set Coord -> Bool
alone = free . neighbors [-1..1] [-1..1]

north :: Coord -> S.Set Coord -> Maybe Coord
north (y,x) ns | northFree (y,x) ns = Just (y-1,x)
               | otherwise = Nothing

east :: Coord -> S.Set Coord -> Maybe Coord
east (y,x) ns | eastFree (y,x) ns = Just (y,x+1)
              | otherwise = Nothing

south :: Coord -> S.Set Coord -> Maybe Coord
south (y,x) ns | southFree (y,x) ns = Just (y+1,x)
               | otherwise = Nothing

west :: Coord -> S.Set Coord -> Maybe Coord
west (y,x) ns | westFree (y,x) ns = Just (y,x-1)
              | otherwise = Nothing

move :: [(Coord -> S.Set Coord -> Maybe Coord)] -> Coord -> S.Set Coord -> Coord
move directions (y,x) elves
  | alone (y,x) elves = (y,x)
  | (n:_) <- mapMaybe (\f -> f (y,x) elves) directions = n
  | otherwise = (y,x)

spreadOut :: Int -> Directions -> Input -> (Directions, Input)
spreadOut times directions elves
  = nTimes times
           (\(dirs, es) ->
             let dirs' = tail dirs <> take 1 dirs
                 moved = M.mapMaybe id
                       . foldr (\elf -> M.insertWith (\_ _ -> Nothing)
                                                     (move dirs elf es)
                                                     (Just elf)
                               )
                               M.empty
                       $ es
                 es' = S.union (M.keysSet moved)
                               (es S.\\ S.fromList (M.elems moved))
              in (dirs', es')
           )
  $ (directions, elves)

emptyTiles :: S.Set Coord -> Int
emptyTiles elves = (1 + yM - ym) * (1 + xM - xm) - S.size elves
  where
    ym = fst . S.findMin $ elves
    yM = fst . S.findMax $ elves
    xs = S.map snd elves
    xm = S.findMin xs
    xM = S.findMax xs

render :: S.Set Coord -> String
render elves = intersperse '\n'
                           [ if (y,x) `S.member` elves then '#' else '.'
                           | y <- [ym..yM]
                           , x <- [xm..xM]
                           ]
  where
    ym = fst . S.findMin $ elves
    yM = fst . S.findMax $ elves
    xs = S.map snd elves
    xm = S.findMin xs
    xM = S.findMax xs

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = emptyTiles . snd . spreadOut 10 [north, south, west, east]
           <$> input
  printAnswer "Empty ground tiles: " answer

roundsToSpread :: Input -> Int
roundsToSpread elves = go 1 [north, south, west, east] elves
  where
    go n dirs es = let (dirs',es') = spreadOut 1 dirs es
                       n' | es == es' = n
                          | otherwise = go (n + 1) dirs' es'
                    in n'

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = roundsToSpread <$> input
  printAnswer "First round where no Elf moves: " answer

main :: IO ()
main = do
  let day = "Day 23: Unstable Diffusion"
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
