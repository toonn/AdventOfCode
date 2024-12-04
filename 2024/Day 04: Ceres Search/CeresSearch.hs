module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import qualified Data.Map as M

type Input = [[Char]]

parser :: Parser Input
parser = characterGrid <* eof

matchingKeys :: (a -> Bool) -> M.Map k a -> [k]
matchingKeys p = M.foldrWithKey (\k a -> if p a then (k:) else id) []

eightWay :: [(Int,Int)]
eightWay = [ (dy,dx) | dy <- [-1..1], dx <- [-1..1], dy /= 0 || dx /= 0 ]

findWithNL :: Ord k => M.Map k Char -> k -> Char
findWithNL m k = M.findWithDefault '\n' k m

xmas :: M.Map YX Char -> YX -> (Int,Int) -> Bool
xmas m (y,x) (dy,dx) = foldr (\(t,c) ->
                               ( c == findWithNL m (y+t*dy,x+t*dx)
                              &&
                               )
                             )
                             True
                             (zip [1..3] "MAS")

xmasOccurrences :: M.Map YX Char -> YX -> Int
xmasOccurrences m yx = foldr (\dyx -> if xmas m yx dyx then (1 +) else id)
                             0
                             eightWay

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum
             . (\m -> map (xmasOccurrences m)
                          (matchingKeys (== 'X') m)
               )
             . foldYX
           <$> input
  printAnswer "XMAS occurrences: " answer

diagonals :: [(Int,Int)]
diagonals = [ (dy,1) | dy <- [1,-1] ]

xedMas :: M.Map YX Char -> YX -> Bool
xedMas m (y,x) = foldr (\(dy,dx) ->
                         ( case both (findWithNL m) ((y+dy,x+dx),(y-dy,x-dx)) of
                             ('M','S') -> True
                             ('S','M') -> True
                             _ -> False
                        &&
                         )
                       )
                       True
                       diagonals


xedMasOccurrences :: M.Map YX Char -> YX -> Int
xedMasOccurrences m yx | xedMas m yx = 1
                       | otherwise = 0

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = sum
             . (\m -> map (xedMasOccurrences m)
                          (matchingKeys (== 'A') m)
               )
             . foldYX
           <$> input
  printAnswer "X-MAS occurrences: " answer

main :: IO ()
main = do
  let day = "Day 04: Ceres Search"
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
