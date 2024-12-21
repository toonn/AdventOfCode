module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Control.Monad (guard)
import Data.List (elemIndex)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S

type Input = [[Char]]

parser :: Parser Input
parser = characterGrid <* eof

holdsOn :: Ord k => (v -> Bool) -> M.Map k v -> k -> Bool
holdsOn p m k | Just v <- m M.!? k = p v
              | otherwise = False

neighbors :: M.Map YX Char -> YX -> [YX]
neighbors grid yx = filter ((`elem` ".ES") `holdsOn` grid)
                           (deltaNeighbors fourWayDeltas yx)

after :: YX -> [YX] -> YX
after from = foldr (\yx -> if yx == from then id else const yx)
                   (error "No neighbors except from")

follow :: (YX -> YX -> YX) -> (YX -> Bool) -> YX -> YX -> [YX]
follow step isGoal prev yx | isGoal yx = [yx]
                           | otherwise = let yx' = step prev yx
                                          in yx : follow step isGoal yx yx'

track :: M.Map YX Char -> [YX]
track grid = let start = foldr (\(yx, t) -> if t == 'S' then const yx else id)
                               (error "No start")
                               (M.assocs grid)
              in follow (\prev yx -> after prev (neighbors grid yx))
                        ((== 'E') `holdsOn` grid)
                        start
                        start

cheats :: M.Map YX Char -> YX -> [(YX,YX)]
cheats grid (y,x) = do dy <- [-1..1]
                       dx <- [-1..1]
                       guard (abs dy /= abs dx)
                       let start = (y - dy, x - dx)
                       let end   = (y + dy, x + dx)
                       guard $ all ((`elem` ".ES") `holdsOn` grid) [start, end]
                       pure (start, end)

dropUntil :: (YX,YX) -> [YX] -> [YX]
dropUntil (start,end) = dropWhile (\yx -> yx /= start && yx /= end)

takeUntil :: (YX,YX) -> [YX] -> [YX]
takeUntil (start,end) = takeWhile (\yx -> yx /= start && yx /= end)

saves :: [YX] -> (YX,YX) -> Int
saves track cheat = let (from : rest) = dropUntil cheat track
                        cut = takeUntil cheat rest
                        saving | from == fst cheat = id
                               | otherwise = negate
                     in saving . length $ cut

neighborWalls :: M.Map YX Char -> YX -> [YX]
neighborWalls grid yx = filter ((`elem` "#") `holdsOn` grid)
                               (deltaNeighbors fourWayDeltas yx)

hundredPSCheats :: M.Map YX Char -> M.Map (YX,YX) Int
hundredPSCheats grid = let t = track grid
                           s = S.fromList t
                           walls = S.fromList (foldMap (neighborWalls grid) t)
                           savings = M.fromSet (saves t)
                                   . foldMap (S.fromList . cheats grid)
                                   $ walls
                        in M.filter (>= 100) $ savings

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = length . hundredPSCheats . foldYX <$> input
  printAnswer "100+ ps cheats: " answer

nr20PSCheats :: [YX] -> Int
nr20PSCheats [] = 0
nr20PSCheats (from:rest) = nr20PSCheats rest
                         + ( length
                           . filter (\to ->
                                      (>= 100)
                                    . (subtract (manhattan from to))
                                    . (+ 1)
                                    . fromJust
                                    . elemIndex to
                                    $ rest
                                    )
                           . filter ((<= 20) . manhattan from)
                           . drop 100
                           $ rest
                           )

-- 8150 too low
part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = (\grid -> nr20PSCheats (track grid)) . foldYX <$> input
  printAnswer "100+ 20 ps cheats: " answer

main :: IO ()
main = do
  let day = "Day 20: Race Condition"
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
