module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import qualified Data.IntMap as IM

type Input = [Int]

parser :: Parser Input
parser = some integer <* space <* eof

blink :: [Int] -> [Int]
blink = foldr (\stone ->
                let ins | stone == 0 = (1:)
                        | let st = show stone
                        , (half, 0) <- length st `quotRem` 2
                        = (read (take half st) :)
                        . (read (drop half st) :)
                        | otherwise = (stone * 2024 :)
                 in ins
              )
              mempty

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = foldBlinks 25 <$> input
  printAnswer "Stones after 25 blinks: " answer

blinks :: IM.IntMap (IM.IntMap Int) -> Int -> Int
       -> (IM.IntMap (IM.IntMap Int), Int)
blinks memo times stone
  | times <= 0 = (memo, stone)
  | otherwise
  = let timeMap = IM.findWithDefault mempty stone memo
        mEntry = timeMap IM.!? times
        Just stones = mEntry
        (memo', times', stones')
          | Nothing <- mEntry
          = let sts = length (blink [stone])
             in (IM.insertWith (<>) stone (IM.singleton 1 sts) memo, 1, sts)
          | otherwise = (memo, times, stones)
        res :: (IM.IntMap (IM.IntMap Int), Int)
        res | times == times' = (memo', stones')
            | otherwise
            = foldr (\s more m ->
                      let (m', ss) = blinks m (times - 1) s
                          (m'', ss') = more m'
                          ss'' = ss + ss'
                       in ( IM.insertWith (<>)
                                          stone
                                          (IM.singleton times ss'')
                                          m''
                          , ss''
                          )
                    )
                    (\m -> (m,0))
                    (blink [stone])
                    memo'
     in res

foldBlinks :: Int -> [Int] -> Int
foldBlinks times stones = foldr (\stone more m ->
                                  let (m', stones') = blinks m times stone
                                   in stones' + more m'
                                )
                                (const 0)
                                stones
                                mempty

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = foldBlinks 75 <$> input
  printAnswer "Stones after 75 blinks: " answer

main :: IO ()
main = do
  let day = "Day 11: Plutonian Pebbles"
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
