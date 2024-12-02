module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

type Input = [[Int]]

report :: Parser [Int]
report = some integer

parser :: Parser Input
parser = sepEndBy1 report eol <* eof

isSafe :: [Int] -> Bool
isSafe r = foldr (\lvlD more sgn ->
                   (signum lvlD + sgn /= 0)
                   && (abs lvlD >= 1 && abs lvlD <= 3)
                   && more (signum lvlD)
                 )
                 (const True)
                 (zipWith (-) (drop 1 r) r)
                 0

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = length . filter id . map isSafe <$> input
  printAnswer "Safe reports: " answer

reinsert :: [a] -> [a] -> [a]
reinsert [] = id
reinsert (x:xs) = reinsert xs . (x:)

isSafeDampened' :: [Int] -> [Int] -> Int -> Bool
isSafeDampened' _ (_:[]) _ = True
isSafeDampened' seen (r1:r2:rest) sgn =
  let direction = signum (r2 - r1)
      dirChange = sgn + direction == 0
      slope = abs (r2 - r1)
      slopeInBounds = slope >= 1 && slope <= 3
      res | dirChange = case seen of
            [] -> isSafe (r1:rest)
            (_:[])  -> isSafe (reinsert (drop 1 seen) (r1:r2:rest))
                    || isSafe (reinsert seen (r1:rest))
                    || isSafe (reinsert seen (r2:rest))
            _ -> isSafe (reinsert seen (r1:rest))
              || isSafe (reinsert seen (r2:rest))
          | slopeInBounds = isSafeDampened' (r1:seen) (r2:rest) direction
          | slope == 0 = isSafe (reinsert seen (r1:rest))
          | otherwise = isSafe (reinsert seen (r1:rest))
                     || isSafe (reinsert seen (r2:rest))
   in res

-- 1 2 3 4
-- 1 2x1x4 If the slope changes either of *three* levels needs to be eliminated.
-- 3x1x4 5
-- 3x2 3 4
-- 1 2 3x1 If the slope is established only the recent level can be eliminated.
-- 3x1x5 6 If the delta is too big one of the two needs to be eliminated.
isSafeDampened :: [Int] -> Bool
isSafeDampened r = isSafeDampened' [] r 0

-- 297 too high
-- 285 too low
-- 292 too low
part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = length . filter id . map isSafeDampened <$> input
  printAnswer "Safe dampened reports: " answer

main :: IO ()
main = do
  let day = "Day 02: Red-Nosed Reports"
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
