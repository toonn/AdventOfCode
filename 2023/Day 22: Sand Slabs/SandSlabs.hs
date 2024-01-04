module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Data.List ((\\),nub,sortOn)

type XYZ = (Int, Int, Int)

type Input = [(XYZ,XYZ)]

block :: Parser (XYZ,XYZ)
block = do
  let comma = char ','
  xyz1 <- (,,) <$> integer <* comma <*> integer <* comma <*> integer
  char '~'
  xyz2 <- (,,) <$> integer <* comma <*> integer <* comma <*> integer
  pure (xyz1, xyz2)

parser :: Parser Input
parser = sepEndBy block eol <* eof

overlap :: (XYZ,XYZ) -> (XYZ,XYZ) -> Bool
overlap ((x1,y1,_),(x2,y2,_)) ((a1,b1,_),(a2,b2,_)) =
  let (xm,xM) = (min x1 x2,max x1 x2)
      (ym,yM) = (min y1 y2,max y1 y2)
      (am,aM) = (min a1 a2,max a1 a2)
      (bm,bM) = (min b1 b2,max b1 b2)
      overlapX = xm <= am && am <= xM
              || xm <= aM && aM <= xM
              || am <= xm && xm <= aM
              || am <= xM && xM <= aM -- redundant but harmless
      overlapY = ym <= bm && bm <= yM
              || ym <= bM && bM <= yM
              || bm <= ym && ym <= bM
              || bm <= yM && yM <= bM -- redundant but harmless
   in overlapX && overlapY

dropBlock :: (XYZ,XYZ) -> Input -> (XYZ,XYZ)
dropBlock block@((x1,y1,z1),(x2,y2,z2))
  = (\dz -> ((x1,y1,dz + 1),(x2,y2,max z1 z2 - min z1 z2 + dz + 1)))
  . maximum
  . (0:)
  . map (\((_,_,z1),(_,_,z2)) -> max z1 z2)
  . filter (overlap block)

settle :: Input -> Input
settle snapshot =
  foldr (\block more settled ->
          more (dropBlock block settled : settled)
        )
        id
        (sortOn (\((_,_,z1),(_,_,z2)) -> min z1 z2) snapshot)
        []

underneath :: (XYZ,XYZ) -> (XYZ,XYZ) -> Bool
underneath ((_,_,z1),(_,_,z2)) ((_,_,c1),(_,_,c2)) = min z1 z2 - max c1 c2 == 1

supporting :: Input -> Int
supporting blocks = length
                  . (blocks \\)
                  . nub
                  . mconcat
                  . filter ((== 1) . length)
                  $ map (\block ->
                          filter (underneath block)
                        . filter (overlap block)
                        $ blocks
                        )
                        blocks

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = supporting . settle <$> input
  printAnswer "Safe bricks to disintegrate: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 22: Sand Slabs"
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
