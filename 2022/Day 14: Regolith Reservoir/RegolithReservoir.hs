module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.List (intercalate)
import qualified Data.IntMap as IM
import qualified Data.Set as S

import AoC

type Coord = (Int, Int)
type Rock = S.Set Coord
type Sand = S.Set Coord
type Blocked = S.Set Coord

newtype Scan = Scan { unScan :: String }

instance Show Scan where
  show = ('\n':) . unScan

type Input = Rock

coord :: Parser Coord
coord = do
  x <- integer
  char ','
  y <- integer
  pure (x, y)

rock :: Parser Rock
rock = S.unions
     . map S.fromAscList
     . (\l -> zipWith (\(x1,y1) (x2,y2) ->
                        case (x1 `compare` x2, y1 `compare` y2) of
                          (LT,EQ) -> zip [x1..x2] (repeat y1)
                          (GT,EQ) -> zip [x2..x1] (repeat y1)
                          (EQ,LT) -> zip (repeat x1) [y1..y2]
                          (EQ,GT) -> zip (repeat x1) [y2..y1]
                      )
                      l
                      (tail l)
       )
   <$> sepBy coord (lexeme (string "->"))

parser :: Parser Input
parser = S.unions <$> manyTill (rock <* eol) eof

sandSource :: Coord
sandSource = (500, 0)

dropUnit :: Rock -> Sand -> Int -> Coord -> Maybe Sand
dropUnit _ _ abyssDepth (_,y) | y > abyssDepth = Nothing
dropUnit rocks sandAtRest abyssDepth (x,y)
  | let down = (x,y+1), S.notMember down blocked
  = dropUnit rocks sandAtRest abyssDepth down
  | let downLeft = (x-1,y+1), S.notMember downLeft blocked
  = dropUnit rocks sandAtRest abyssDepth downLeft
  | let downRight = (x+1,y+1), S.notMember downRight blocked
  = dropUnit rocks sandAtRest abyssDepth downRight
  where blocked = rocks `S.union` sandAtRest
dropUnit _ sandAtRest _ unit = Just (S.insert unit sandAtRest)

simulateSand :: Rock -> Sand
simulateSand rocks = foldr (\unit more sandAtRest ->
                             case dropUnit rocks sandAtRest abyssDepth unit of
                               Nothing -> sandAtRest
                               Just sandAtRest' -> more sandAtRest'
                           )
                           id
                           (repeat sandSource)
                           S.empty
  where
    abyssDepth = S.findMax . S.map snd $ rocks

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = S.size . simulateSand <$> input
  printAnswer "Units at rest before sand flows into the abyss: " answer

{- Good effort, but no.
dropWithFloor :: Rock -> Sand -> Int -> Coord -> Maybe Sand
dropWithFloor _ sandAtRest _ unit | unit `S.member` sandAtRest = Nothing
dropWithFloor rocks sandAtRest floorDepth (x,y)
  | y + 1 == floorDepth = Just (S.insert (x,y) sandAtRest)
  | let down = (x,y+1), S.notMember down blocked
  = dropWithFloor rocks sandAtRest floorDepth down
  | let downLeft = (x-1,y+1), S.notMember downLeft blocked
  = dropWithFloor rocks sandAtRest floorDepth downLeft
  | let downRight = (x+1,y+1), S.notMember downRight blocked
  = dropWithFloor rocks sandAtRest floorDepth downRight
  where blocked = rocks `S.union` sandAtRest
dropWithFloor _ sandAtRest _ unit = Just (S.insert unit sandAtRest)

simulateWithFloor :: Rock -> Sand
simulateWithFloor rocks = foldr (\unit more sandAtRest ->
                             case (dropWithFloor rocks
                                                 sandAtRest
                                                 floorDepth
                                                 unit
                                  ) of
                               Nothing -> sandAtRest
                               Just sandAtRest' -> more sandAtRest'
                           )
                           id
                           (repeat sandSource)
                           S.empty
  where
    abyssDepth = S.findMax . S.map snd $ rocks
    floorDepth = abyssDepth + 2
-}

pyramid :: Int -> Coord -> Sand
pyramid floorDepth (x,y)
  | y + 1 == floorDepth = S.singleton (x,y)
  | otherwise = S.unions [ pyramid floorDepth (x, y + 1)
                         , diagonal (x - 1, y + 1) (-)
                         , diagonal (x + 1, y + 1) (+)
                         , S.singleton (x,y)
                         ]
  where
    diagonal (x,y) op = S.fromAscList
                      . map (\n -> (x `op` n, y + n))
                      $ let nMax = floorDepth - y - 1
                            ns | x `op` 1 < x = [nMax,nMax - 1..0]
                               | otherwise = [0..nMax]
                         in ns

yToXMap :: Rock -> IM.IntMap (S.Set Int)
yToXMap = S.foldr (\(x,y) -> IM.insertWith (S.union) y (S.singleton x)) IM.empty

xMapToSet :: IM.IntMap (S.Set Int) -> Rock
xMapToSet = IM.foldrWithKey (\y xs rocks ->
                              S.map (\x -> (x,y)) xs `S.union` rocks
                            )
                            S.empty

consecutive :: S.Set Int -> [S.Set Int]
consecutive xs | S.null xs = []
               | otherwise
               = let (l,xs') = S.deleteFindMin xs
                     rows = consecutive xs'
                     adjacents | (adjacent:others) <- rows
                               , (l + 1) `S.member` adjacent
                               = S.insert l adjacent : others
                               | otherwise = S.singleton l : rows
                  in adjacents

rockBlockages :: Int -> Rock -> Rock
rockBlockages floorDepth rocks
  = xMapToSet
  $ foldr (\y more blocked ->
            let blockedXs = IM.lookup y blocked
                blocked'
                  | Nothing <- blockedXs = more blocked
                  | Just xs <- blockedXs
                  = more (IM.insertWith (S.union)
                                        (y+1)
                                        ( S.unions
                                        . map (S.deleteMin . S.deleteMax)
                                        . consecutive
                                        $ xs
                                        )
                                        blocked
                         )
             in blocked'
          )
          id
          [0..floorDepth - 1]
          (yToXMap rocks)

simulateWithFloor :: Rock -> Sand
simulateWithFloor rocks = pyramid floorDepth sandSource
                     S.\\ rockBlockages floorDepth rocks
  where
    abyssDepth = S.findMax . S.map snd $ rocks
    floorDepth = abyssDepth + 2

renderCoords :: Rock -> Sand -> Scan
renderCoords rock sand = Scan
                       $ intercalate "\n"
                                     [ map render [(x,y) | x <- [xMin..xMax]]
                                     | y <- [0..yMax]
                                     ]
  where
    everything = rock `S.union` sand
    (xMin, _) = S.findMin everything
    (xMax, _) = S.findMax everything
    yMax = S.findMax . S.map snd $ everything

    render c | c `S.member` rock = '#'
             | c `S.member` sand = 'o'
             | otherwise = '.'

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = S.size . simulateWithFloor <$> input
  printAnswer "Units at rest when source blocked: " answer

main :: IO ()
main = do
  let day = "Day 14: Regolith Reservoir"
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
