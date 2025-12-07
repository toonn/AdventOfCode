module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Data.Foldable (foldMap')
import qualified Data.Set as S

type Input = (S.Set Int,[S.Set Int])

parser :: Parser Input
parser = (\(start:splitters) -> (start, splitters))
       . map (S.fromAscList . map fst . filter ((/= '.') . snd) . zip [0..])
     <$> sepEndBy1 (takeWhile1P (Just "One of S . ^") (/= '\n')) eol
      <* eof

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = fst
             . (\(start, splitters) ->
                 foldr (\splits next (count, beams) ->
                         let hits = S.intersection beams splits
                             misses = beams S.\\ splits
                          in next ( count + length hits
                                  , S.union
                                      misses
                                      (foldMap'
                                        (\s ->
                                          S.map (s +) (S.fromAscList [-1,1]))
                                        hits
                                      )
                                  )
                       )
                       id
                       splitters
                       (0, start)
               )
           <$> input
  printAnswer "Number of splits: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 07: Laboratories"
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
