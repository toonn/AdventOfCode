module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec hiding (chunk)
import Text.Megaparsec.Char

import Control.Monad (void)
import Data.List -- (sort)
import qualified Data.IntMultiSet as B
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Set as S

import AoC

type Coord = (Int, Int, Int)
type Scan = [Coord]
type Scans = [Scan]
type Bag = B.IntMultiSet

coord :: Parser Coord
coord = (\[x,y,z] -> (x,y,z)) <$> sepBy1 (signed integer) (char ',') <* eol

scanner :: Parser Scan
scanner = do
  string "--- scanner "
  integer
  string "---"
  eol
  manyTill coord (void eol <|> eof)

parser :: Parser Scans
parser = manyTill (scanner) eof

distance :: Coord -> Coord -> Int
distance (a,b,c) (d,e,f) = abs (a - d) + abs (b - e) + abs (c - f)

patterns :: Scan -> [Bag]
patterns scan = foldr
  (\center ps -> B.fromList (map (distance center) scan) : ps)
  []
  scan

overlap :: Scan -> Scan -> S.Set (Coord, Coord)
overlap as bs = S.fromList
  (foldr (\(ac, p) cs ->
    foldr (\(bc, q) cs' ->
            if (B.size (p `B.intersection` q)) >= 12
            then (ac, bc):cs'
            else cs'
          )
          cs
          (zip bs (patterns bs))
    )
    []
    (zip as (patterns as))
  )

overlapping :: Scans -> [(Int, Int)]
overlapping =  (\pss ->
                 foldr (\(i,ps) overlaps ->
                         foldr (\(j, qs) os ->
                                 if i < j && S.size (overlap ps qs) >= 12
                                 then (i,j):os
                                 else os
                               )
                               overlaps
                         . filter (\(j,_) -> j /= i)
                         $ pss
                       )
                       []
                       pss
               )
               . zip [0..]

rotations :: [Coord -> Coord]
rotations = [ \(a,b,c) -> ( a, b, c)
            , \(a,b,c) -> ( a, c,-b)
            , \(a,b,c) -> ( a,-b,-c)
            , \(a,b,c) -> ( a,-c, b)
            , \(a,b,c) -> (-a, b,-c)
            , \(a,b,c) -> (-a, c, b)
            , \(a,b,c) -> (-a,-b, c)
            , \(a,b,c) -> (-a,-c,-b)
            , \(a,b,c) -> (-b, a, c)
            , \(a,b,c) -> (-c, a,-b)
            , \(a,b,c) -> ( b, a,-c)
            , \(a,b,c) -> ( c, a, b)
            , \(a,b,c) -> (-c,-a, b)
            , \(a,b,c) -> ( b,-a, c)
            , \(a,b,c) -> ( c,-a,-b)
            , \(a,b,c) -> (-b,-a,-c)
            , \(a,b,c) -> (-c, b, a)
            , \(a,b,c) -> ( b, c, a)
            , \(a,b,c) -> ( c,-b, a)
            , \(a,b,c) -> (-b,-c, a)
            , \(a,b,c) -> ( c, b,-a)
            , \(a,b,c) -> (-b, c,-a)
            , \(a,b,c) -> (-c,-b,-a)
            , \(a,b,c) -> ( b,-c,-a)
            ]

difference :: Coord -> Coord -> Coord
difference (a,b,c) (d,e,f) = (a-d, b-e, c-f)

addition :: Coord -> Coord -> Coord
addition (a,b,c) (d,e,f) = (a+d, b+e, c+f)

inverse :: (Coord -> Coord) -> (Coord -> Coord)
inverse t = addition translation' . rotation'
  where
    translation = t (0,0,0)
    rotation = t (1,2,3) `difference` translation
    rotation' (a,b,c) = case rotation of
      ( 1, 2, 3) -> ( a, b, c)
      ( 1, 3,-2) -> ( a,-c, b)
      ( 1,-2,-3) -> ( a,-b,-c)
      ( 1,-3, 2) -> ( a, c,-b)
      (-1, 2,-3) -> (-a, b,-c)
      (-1, 3, 2) -> (-a, c, b)
      (-1,-2, 3) -> (-a,-b, c)
      (-1,-3,-2) -> (-a,-c,-b)
      (-2, 1, 3) -> ( b,-a, c)
      (-3, 1,-2) -> ( b,-c,-a)
      ( 2, 1,-3) -> ( b, a,-c)
      ( 3, 1, 2) -> ( b, c, a)
      (-3,-1, 2) -> (-b, c,-a)
      ( 2,-1, 3) -> (-b, a, c)
      ( 3,-1,-2) -> (-b,-c, a)
      (-2,-1,-3) -> (-b,-a,-c)
      (-3, 2, 1) -> ( c, b,-a)
      ( 2, 3, 1) -> ( c, a, b)
      ( 3,-2, 1) -> ( c,-b, a)
      (-2,-3, 1) -> ( c,-a,-b)
      ( 3, 2,-1) -> (-c, b, a)
      (-2, 3,-1) -> (-c,-a, b)
      (-3,-2,-1) -> (-c,-b,-a)
      ( 2,-3,-1) -> (-c, a,-b)
    translation' = (0,0,0) `difference` rotation' translation

transformations :: [((Int, Int), S.Set (Coord, Coord))]
                -> IM.IntMap (Coord -> Coord)
transformations overlaps = go pairTransformations (IM.singleton 0 id)
  where
    pairTransformations =
      foldr (\(ij, ijs) ts ->
              let is = S.foldr (\p ps -> fst p : ps) [] ijs
                  js = S.foldr (\p ps -> snd p : ps) [] ijs
                  j's = map (\f -> map (\x -> f x) js) rotations
                  iDifferences = map (uncurry difference) (zip is (tail is))
                  j'Differences = map ( map (uncurry difference)
                                      . (\js' -> zip js' (tail js'))
                                      )
                                      j's
                  rotation = case elemIndex iDifferences j'Differences of
                    Nothing -> error "Can't find rotation!"
                    Just r -> rotations !! r
                  translation = case S.toList ijs of
                    [] -> error "No coordinates to find translation!"
                    ((c, c'):_) -> difference c (rotation c')
               in M.insert ij (addition translation . rotation) ts
            )
            M.empty
            overlaps

    go pTs ts | M.null pTs = ts
              | otherwise = case (M.foldrWithKey
        (\(kf,kt) t (pTs', ts') ->
          if kf == 0
          then (pTs', IM.insert kt t ts')
          else case IM.lookup kf ts' of
                 Just t' -> (pTs', IM.insert kt (t' . t) ts')
                 Nothing -> (M.insert (kf,kt) t pTs', ts')
        )
        (M.empty, ts)
        pTs
      ) of
        (pTs', ts')
          | IM.keys ts' == IM.keys ts -> case (M.foldrWithKey
              (\(kf,kt) t (pTs', ts') ->
                (M.insert (kt,kf) (inverse t) pTs', ts')
              )
              (M.empty, ts)
              pTs
            ) of
              (pTs', ts') -> go pTs' ts'
          | otherwise -> go pTs' ts'

scannerTransformations :: Scans -> [(Int, Int)] -> IM.IntMap (Coord -> Coord)
scannerTransformations scans overlappingScanners = transformations overlaps
  where
    overlaps = foldr (\(i,j) os ->
                       ((i,j),overlap (scans !! i) (scans !! j)) : os
                     )
                     []
                     overlappingScanners

beacons :: Scans -> [(Int, Int)] -> S.Set Coord
beacons scans overlappingScanners = zeroReferenced
  where
    ts = scannerTransformations scans overlappingScanners

    zeroReferenced = S.fromList
      (concatMap (\(i,s) -> case IM.lookup i ts of
                              Nothing -> error "Can't transform!"
                              Just t -> map t s
                 )
                 (zip [0..] scans)
      )

part1 :: Parsed Scans -> IO ()
part1 input = do
  let answer = S.size . (\scans -> beacons scans (overlapping scans)) <$> input
  printAnswer "Number of beacons: " answer

manhattan :: Coord -> Coord -> Int
manhattan (a,b,c) (d,e,f) = abs (a - d) + abs (b - e) + abs (c - f)

greatestDistance :: IM.IntMap (Coord -> Coord) -> Int
greatestDistance ts = go scanners 0
  where
    scanners = (0, 0, 0) : IM.foldr (\t ss -> t (0,0,0) : ss) [] ts

    go [c] m = m
    go (c:cs) m | m' <- maximum . map (manhattan c) $ cs =
      if m' < m then go cs m else go cs m'

part2 :: Parsed Scans -> IO ()
part2 input = do
  let answer = greatestDistance
             . (\scans -> scannerTransformations scans (overlapping scans))
           <$> input
  printAnswer "Largest Manhattan distance: " answer

main :: IO ()
main = do
  let day = "Day 19: Beacon Scanner"
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
