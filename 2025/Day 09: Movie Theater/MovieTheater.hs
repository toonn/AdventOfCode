module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Control.Monad (join)
import Data.Foldable (foldMap')
import Data.List (sortOn, tails)
import qualified Data.Set as S

type Input = [YX]

parser :: Parser Input
parser = sepEndBy1 ((,) <$> integer <* char ',' <*> integer) eol <* eof

area :: YX -> YX -> Int
area (y1,x1) (y2,x2) = (abs (y1 - y2) + 1) * (abs (x1 - x2) + 1)

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = foldr (\(xy:xys) -> max . maximum . map (area xy) $ xys) 0
             . filter atLeastTwo
             . tails
           <$> input
  printAnswer "Largest rectangle: " answer

atLeastTwo :: [a] -> Bool
atLeastTwo []  = False
atLeastTwo [_] = False
atLeastTwo _   = True

bounds :: S.Set YX -> (YX, YX)
bounds borderS | null borderS = error "Empty space doesn't have bounds"
               | xs <- S.map snd borderS
               = ( (fst (S.findMin borderS), S.findMin xs)
                 , (fst (S.findMax borderS), S.findMax xs)
                 )

areaSet :: YX -> YX -> S.Set YX
areaSet (y1,x1) (y2,x2) = S.fromList [ (y,x) | y <- [min y1 y2..max y1 y2]
                                             , x <- [min x1 x2..max x1 x2]
                                     ]

border :: Input -> S.Set YX
border = foldMap' (uncurry areaSet) . join (zip . (\(yx:yxs) -> yxs <> [yx]))

corners :: (YX,YX) -> [YX]
corners ((y1,x1),(y2,x2)) = [(y1,x1),(y1,x2),(y2,x2),(y2,x1)]

outOfBounds :: (YX,YX) -> YX -> Bool
outOfBounds ((ym,xm),(yM,xM)) (y,x) = y < ym || y > yM || x < xm || x > xM

neighbors :: YX -> S.Set YX
neighbors = S.fromAscList . deltaNeighbors fourWayDeltas

-- Assume there are no exterior regions that do not touch the bounds.
-- Problematic example: #XXX#
--                      X#X#X
--                      XX.##
--                      X#XX#
--                      #XXX#
contained :: S.Set YX -> S.Set YX -> (YX,YX) -> S.Set YX
contained borderS bad pair = go (rectangleBorder <> borderS <> front) front
  where
    cs = corners pair
    rectangleBorder = border cs
    front = S.filter (outOfBounds (bounds (S.fromList cs)))
          . foldMap' neighbors
          $ rectangleBorder S.\\ borderS

    go :: S.Set YX -> S.Set YX -> S.Set YX
    go seen candidates | (not . null $ S.intersection bad candidates)
                       || any (outOfBounds (bounds borderS)) ns
                       = bad <> candidates <> (seen <> uncurry areaSet pair
                                           S.\\ borderS
                                              )
                       | null ns = mempty
                       | otherwise = go (seen <> ns) ns
      where
        ns = foldMap' neighbors candidates S.\\ seen

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = (\(pair:_) -> uncurry area pair)
             . (\tiles -> (\pairs ->
                            foldr (\pair next bad ->
                                    case contained (border tiles) bad pair of
                                      bad' | null bad' -> pair : next bad
                                           | otherwise -> next bad'
                                  )
                                  (const [])
                                  pairs
                                  mempty
                          )
                        . sortOn (negate . uncurry area)
                        . foldMap (\(yx:yxs) -> map ((,) yx) yxs)
                        . filter atLeastTwo
                        . tails
                        $ tiles
               )
           <$> input
  printAnswer "Largest red and green rectangle: " answer

main :: IO ()
main = do
  let day = "Day 09: Movie Theater"
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
