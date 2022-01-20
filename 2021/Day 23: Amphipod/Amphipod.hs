module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec hiding (chunk)
import Text.Megaparsec.Char

import Data.List (sort)
import qualified Data.Map as M
import qualified Data.PQueue.Min as PQ
import qualified Data.Set as S

import AoC

type Coord = (Int, Int)
type Amphipod = (Char, Coord)
type Amphipods = [Amphipod]
type Steps = PQ.MinQueue (Int, Amphipods)

homes :: [Int]
homes = [2,4,6,8]

parser :: Parser Amphipods
parser = do
  many (char '#')
  eol
  hallway <- between (char '#') (char '#') (many (char '.'))
  eol
  amphipods <- filter (`elem` "ABCD")
           <$> manyTill (satisfy (`elem` " #ABCD\n")) eof
  pure (zip amphipods [(x,y) | y <- [1,2], x <- homes])

manhattan :: Coord -> Coord -> Int
manhattan (x,y) (x',y') = abs (x - x') + abs (y -y')

home :: Char -> Coord
home = (M.fromAscList (zip "ABCD" [(x,2) | x <- homes]) M.!)

cost :: Char -> Int
cost = (M.fromAscList (zip "ABCD" [1,10,100,1000]) M.!)

heuristic :: Amphipods -> Int
heuristic = sum . map (\(a, (x,y)) ->
                        let h@(hx,_) = home a
                            correction | x == hx = -y
                                       | otherwise = y
                         in cost a * (manhattan (x,0) h + correction)
                      )

interleave :: [a] -> [a] -> [a]
interleave as [] = as
interleave [] bs = bs
interleave (a:as) bs = a : interleave bs as

path :: Coord -> Coord -> S.Set Coord
path (a,b) (c,d) = S.fromAscList ( zip (repeat x1) [0..y1]
                                <> zip [x1+1..x2-1] (repeat 0)
                                <> zip (repeat x2) [0..y2]
                                 )
              S.\\ S.singleton (a,b)
  where
    (x1,y1,x2,y2) | a < c     = (a,b,c,d)
                  | otherwise = (c,d,a,b)

update :: Amphipod -> Coord -> Amphipods -> Amphipods
update (a, xy) xy' = sort
                   . foldr (\a' ->
                             let a'' | (a,xy) == a' = (a,xy')
                                     | otherwise = a'
                              in (a'':)
                           )
                           []

unobstructed :: Amphipods -> Coord -> Coord -> Bool
unobstructed as p1 p2 = S.null (S.intersection (path p1 p2)
                                               (S.fromList (map snd as))
                               )

unobstructedMoves :: Amphipods -> Coord -> [Int] -> [Coord]
unobstructedMoves as xy = takeWhile (\xy' -> unobstructed as xy xy')
                        . map (\x' -> (x',0))
                        . filter (not . (`elem` homes))

-- Make sure blocked positions are taken into account.
moves :: Amphipods -> Amphipod -> [(Int, Amphipods)]
moves as (a, xy@(x,y))
  | y == 0 = let g | (a, (hx,hy)) `elem` as = (hx,1)
                   | otherwise = (hx,hy)
                 ms | unobstructed as xy g = [ ( cost a * manhattan xy g
                                               , update (a, xy) g as
                                               )
                                             ]
                    | otherwise = []
              in ms
  | y /= 0 = map (\g -> ( cost a * manhattan xy g
                        , update (a, xy) g  as
                        )
                 )
                 xys
    where
      (hx,hy) = home a

      leftward = unobstructedMoves as xy [x, x - 1..0]
      rightward = unobstructedMoves as xy [x..10]

      xys = interleave leftward rightward

neighbors :: Amphipods -> [(Int, Amphipods)]
neighbors as = concatMap (moves as) as

-- A*
search :: Amphipods -> Int
search as = go openSet gScore
  where
    as' = sort as
    openSet = PQ.singleton (heuristic as', as')
    gScore = M.singleton as' 0
    goal = [(a, (x,y)) | (a,x) <- zip "ABCD" homes, y <- [1,2]]

    go :: Steps -> M.Map Amphipods Int -> Int
    go oS gS | current == goal = gS M.! goal
             | otherwise = uncurry go
               (foldr (\(c, nAs) (pOS, pGS) ->
                        let tentative_gS = pGS M.! current + c
                            (nOS, nGS) | Just n_gS <- pGS M.!? nAs
                                       , tentative_gS >= n_gS
                                       = (pOS, pGS)
                                       | otherwise
                                       = ( PQ.insert ( tentative_gS
                                                     + heuristic nAs
                                                     , nAs
                                                     )
                                                     pOS
                                         , M.insert nAs tentative_gS pGS
                                         )
                         in (nOS, nGS)
                      )
                      (oS', gS)
                      (neighbors current)
               )
      where
        ((c, current), oS') = PQ.deleteFindMin oS

part1 :: Parsed Amphipods -> IO ()
part1 input = do
  let answer = search <$> input
  printAnswer "Least energy to organize amphipods: " answer

part2 :: Parsed Amphipods -> IO ()
part2 input = do
  let answer = const "P" <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 23: Amphipod"
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
