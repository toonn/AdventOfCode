module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Control.Monad (join)
import Data.Foldable (foldMap', toList)
import Data.List (mapAccumL, sort, sortBy)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tuple (swap)

type XYZ = (Int, Int, Int)

type Input = S.Set (S.Set XYZ)

parser :: Parser Input
parser = S.fromList
       . map S.singleton
     <$> sepEndBy1 ((,,) <$> integer <* char ','
                         <*> integer <* char ','
                         <*> integer
                   )
                   eol
      <* eof

euclidean :: XYZ -> XYZ -> Double
euclidean (p1,p2,p3) (q1,q2,q3) = sqrt
                                . fromIntegral
                                . sum
                                . map (^ 2)
                                $ zipWith (-) [p1,p2,p3] [q1,q2,q3]

closestFirst :: Input -> [S.Set XYZ]
closestFirst circuits = map snd
                      . sort
                      . map swap
                      . M.assocs
                      . M.fromSet (\s -> euclidean (S.findMin s) (S.findMax s))
                      . S.filter ((== 2) . length)
                      . join (foldMap' . flip (S.map . S.union))
                      $ circuits

connect :: Input -> S.Set XYZ -> Input
connect circuits closest = let (others, close)
                                 = S.partition (null . S.intersection closest)
                                               circuits
                            in S.insert (S.unions close) others

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = product
             . take 3
             . sortBy (flip compare)
             . map length
             . toList
             . join (flip (foldl' connect) . take 1000 . closestFirst)
           <$> input
  printAnswer "Size product after 1000 connections: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = product
             . S.map (\(x,_,_) -> x)
             . join ( (foldr (\closest next circuits ->
                               let connected = connect circuits closest
                                   lastConnect | length connected == 1 = closest
                                               | otherwise = next connected
                                in lastConnect
                             )
                             (error "Never connect all junction boxes")
                      )
                    . closestFirst
                    )
           <$> input
  printAnswer "Product of Xs of last connection: " answer

main :: IO ()
main = do
  let day = "Day 08: Playground"
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
