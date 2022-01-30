module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec hiding (chunk)
import Text.Megaparsec.Char

import qualified Data.Map as M
import qualified Data.Set as S

import AoC

type Coord = (Int, Int)
type Bounds = Coord
type Cucumbers = (S.Set Coord, S.Set Coord)

newtype Strung = Strung { unStrung :: String }

instance Show Strung where
  show = unStrung

instance Semigroup Strung where
  (Strung a) <> (Strung b) = Strung (a <> b)

cucumbers :: Int -> Int -> Cucumbers -> Parser (Int, Cucumbers)
cucumbers y x cs@(east,south) = do
  (eol *> pure (x, cs))
  <|> char '.' *> cucumbers y (x+1) cs
  <|> char '>' *> cucumbers y (x+1) (S.insert (x,y) east, south)
  <|> char 'v' *> cucumbers y (x+1) (east, S.insert (x,y) south)

row :: Int -> Cucumbers -> Parser (Bounds, Cucumbers)
row y cs = do
  (bX, cs') <- cucumbers y 0 cs
  (eof *> pure ((bX,y + 1), cs')) <|> row (y + 1) cs'

parser :: Parser (Bounds, Cucumbers)
parser = row 0 (S.empty, S.empty)

move :: (Coord -> Coord) -> S.Set Coord -> S.Set Coord -> S.Set Coord
move direction mobile fixed =
  (\m -> M.keysSet m `S.union` (mobile S.\\ (S.fromList (M.elems m))))
  . (\m -> M.withoutKeys m (mobile `S.union` fixed))
  . M.mapKeys snd
  . M.fromSet fst
  . S.map (\c -> (c, direction c))
  $ mobile

step :: Bounds -> Cucumbers -> Cucumbers
step (bX,bY) (east,south) = (east', south')
  where
    east' = move (\(x,y) -> ((x + 1) `rem` bX, y)) east south
    south' = move (\(x,y) -> (x, (y + 1) `rem` bY)) south east'

render :: Bounds -> Cucumbers -> Strung
render (bX,bY) (east,south) =
 Strung
 . ('\n':)
 . foldr (\((x,y), (pX,pY)) s ->
           let nl | pY > y = "\n"
                  | otherwise = ""
               c | (x,y) `S.member` east = '>'
                 | (x,y) `S.member` south = 'v'
                 | otherwise = '.'
            in c : nl <> s
         )
         ""
 . (\cs -> zip cs (tail cs <> [(0,bY)]))
 $ [(x,y) | y <- [0..bY - 1], x <- [0..bX - 1]]

part1 :: Parsed (Bounds, Cucumbers) -> IO ()
part1 input = do
  let answer = (\cs -> foldr (\(n,nCs) next (p,pCs) ->
                               -- Has to be n because the first step where
                               -- no cucumbers move is the first step that has
                               -- occured before, not the one that will occur
                               -- again.
                               if pCs == nCs then n else next (n, nCs)
                             )
                             fst
                             cs
                             (0,(S.empty,S.empty))
               )
             . zip [0..]
             . (\(bounds, cs) -> iterate (step bounds) cs)
           <$> input
  printAnswer "First step with no movement: " answer

part2 :: Parsed (Bounds, Cucumbers) -> IO ()
part2 input = do
  let answer = const "P" <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 25: Sea Cucumber"
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
