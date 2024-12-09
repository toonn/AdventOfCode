module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Data.Char (digitToInt, isDigit)
import qualified Data.IntMap as IM

type Input = [Int]

parser :: Parser Input
parser = map digitToInt <$> takeWhile1P (Just "digit") isDigit <* space <* eof

sizeMap :: [Int] -> (IM.IntMap Int, IM.IntMap Int)
sizeMap = both (IM.fromAscList . zip [0..])
        . foldr (\s (as,bs) -> (s:bs,as)) (mempty,mempty)

fitFree :: IM.IntMap Int -> Int -> ([Int], IM.IntMap Int)
fitFree fileBlocks 0 = ([], fileBlocks)
fitFree fileBlocks freeSize
  = let mF = IM.maxViewWithKey fileBlocks
        Just ((fID,fSize), fileBlocks') = mF
        res | Nothing <- mF = ([],mempty)
            | freeSize < fSize = ( replicate freeSize fID
                                 , IM.insert fID (fSize - freeSize) fileBlocks'
                                 )
            | (fs,bs) <- fitFree fileBlocks' (freeSize - fSize)
            = (replicate fSize fID <> fs, bs)
     in res

compact :: (IM.IntMap Int, IM.IntMap Int) -> [Int]
compact (fileBlocks,freeBlocks)
  = let mF = IM.minViewWithKey fileBlocks
        Just ((fID,fSize),fileBlocks') = mF
        mFr = IM.minViewWithKey freeBlocks
        Just ((_,freeSize),freeBlocks') = mFr
        (fitBlocks,fileBlocks'') | Nothing <- mFr = ([],fileBlocks')
                                 | otherwise = fitFree fileBlocks' freeSize
        blocks | Nothing <- mF = []
               | otherwise = replicate fSize fID
                          <> fitBlocks
                          <> compact (fileBlocks'',freeBlocks')
     in blocks

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . zipWith (*) [0..] . compact . sizeMap <$> input
  printAnswer "Compacted checksum: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 09: Disk Fragmenter"
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
