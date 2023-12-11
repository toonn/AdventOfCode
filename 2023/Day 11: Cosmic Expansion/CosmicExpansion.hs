module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AoC

import qualified Data.Set as S

type YX = (Int, Int)

type Input = S.Set YX

row :: Parser [Int]
row = (\r -> foldr (\pixel more x -> let gs | pixel == '#' = (x:)
                                            | otherwise    = id
                                      in gs (more (x + 1))
                   ) (const []) r 0) <$> some (oneOf ".#")

parser :: Parser Input
parser = S.fromAscList . concat . zipWith (map . (,)) [0..]
     <$> sepEndBy1 row eol <* eof

expand :: Int -> Input -> Input
expand expansion galaxies =
  S.map (\(y,x) ->
          let y' = y + (expansion - 1) * length (filter (< y) emptyYs)
              x' = x + (expansion - 1) * length (filter (< x) emptyXs)
           in (y',x')
        )
        galaxies
  where
    ys = S.map fst galaxies
    (minY,maxY) = (S.findMin ys, S.findMax ys)
    xs = S.map snd galaxies
    (minX,maxX) = (S.findMin xs, S.findMax xs)
    emptyYs = filter (`S.notMember` ys) [minY..maxY]
    emptyXs = filter (`S.notMember` xs) [minX..maxY]

pairs :: [YX] -> [(YX, YX)]
pairs []  = []
pairs [_] = []
pairs (g:galaxies) = map ((,) g) galaxies <> pairs galaxies

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . map (uncurry manhattan) . pairs . S.toList . expand 2
           <$> input
  printAnswer "Sum of all pairwise shortest paths: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = sum . map (uncurry manhattan) . pairs . S.toList . expand 1000000
           <$> input
  printAnswer "Sum of pairwise shortest paths in older universe: " answer

main :: IO ()
main = do
  let day = "Day 11: Cosmic Expansion"
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
