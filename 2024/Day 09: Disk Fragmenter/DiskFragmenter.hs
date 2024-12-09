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

checksum :: [Int] -> Int
checksum = sum . zipWith (*) [0..]

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = checksum . compact . sizeMap <$> input
  printAnswer "Compacted checksum: " answer

compactFiles' :: IM.IntMap Int -> IM.IntMap Int
              -> (IM.IntMap Int, IM.IntMap Int, IM.IntMap [Int])
compactFiles' fileBlocks freeBlocks
  = let mF = IM.maxViewWithKey fileBlocks
        Just ((fID,fSize),fileBlocks') = mF
        noop = (IM.insert fID fSize, freeBlocks, mempty)
        (fileIns, freeBlocks', fittedBlock) = IM.foldrWithKey
          (\freeID freeSize next ->
            let fitted = IM.singleton freeID (replicate fSize fID)
                insFree = IM.insertWith (+) (fID - 1) fSize
                ins = case fSize `compare` freeSize of
                        LT -> ( id
                              , insFree (IM.adjust (subtract fSize)
                                                   freeID
                                                   freeBlocks
                                        )
                              , fitted
                              )
                        EQ -> ( id
                              , insFree (IM.delete freeID freeBlocks)
                              , fitted
                              )
                        GT -> next
                res | freeID < fID = ins
                    | otherwise = noop
             in res
          )
          noop
          freeBlocks
        res | Nothing <- mF = (mempty, freeBlocks, mempty)
            | (files, frees, fits) <- compactFiles' fileBlocks' freeBlocks'
            = (fileIns files, frees, IM.unionWith (<>) fittedBlock fits)
     in res

represent :: IM.IntMap Int -> IM.IntMap [Int]
represent = IM.mapWithKey (flip replicate)

compactFiles :: (IM.IntMap Int, IM.IntMap Int) -> [Int]
compactFiles = mconcat
             . IM.elems
             . (\(files, frees, fits) ->
                 IM.unionsWith (<>) [ represent files
                                    , fits
                                    , fmap (flip replicate 0) frees
                                    ]
               )
             . uncurry compactFiles'

-- 6363235997198 too low
part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = checksum . compactFiles . sizeMap <$> input
  printAnswer "Whole file compact checksum: " answer

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
