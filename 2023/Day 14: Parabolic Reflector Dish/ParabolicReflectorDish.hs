module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Control.Arrow (first, second)
import qualified Data.Map as M
import qualified Data.Set as S

type YX = (Int, Int)
type Platform = (YX, S.Set YX, S.Set YX)
data Direction = North | East | South | West deriving Eq

type Input = [[Char]]

parser :: Parser Input
parser =
  sepEndBy1 (takeWhile1P (Just "Rock or empty space") (`elem` ".#O")) eol <* eof

mkPlatform :: Input -> Platform
mkPlatform rows = (\(ms,ss) -> ((length rows, length (head rows)), ms, ss))
                $ foldr (\row more y rocks ->
                          more (y + 1)
                        $ foldr (\r more x rocks ->
                                  let insYX = S.insert (y,x)
                                      addYX | r == 'O' = first insYX
                                            | r == '#' = second insYX
                                            | otherwise = id
                                   in more (x + 1) (addYX rocks)
                                )
                                (flip const)
                                row
                                0
                                rocks
                        )
                        (flip const)
                        rows
                        0
                        (S.empty, S.empty)

groupByIndex :: Foldable f
             => (a -> Int)
             -> ((a -> Bool) -> f a -> (f a, f a))
             -> f a
             -> [f a]
groupByIndex toI partition foldable =
  foldr (\i more f ->
          let (is, f') = partition ((== i) . toI) f
              rest | null f' = []
                   | otherwise = more f'
           in is : rest
        )
        (error "Infinite lists shouldn't end")
        [0..]
        foldable

shift :: Direction -> Int -> S.Set Int -> S.Set Int -> S.Set Int
shift dir edge mobile stationary
  | null mobile = S.empty
  | otherwise = shifted <> shiftedRest
  where
    mExtreme | dir `elem` [East, South] = S.minView stationary
             | otherwise = S.maxView stationary
    extreme | Just (e,_) <- mExtreme = e
            | otherwise = edge
    (mLess, mMore) = S.split extreme mobile
    shifted | dir `elem` [North, West]
            = S.fromAscList . map (extreme +) $ [1..length mMore]
            | otherwise
            = S.fromDescList . map (extreme -) $ [1..length mLess]
    mRest | dir `elem` [North, West] = mLess
          | otherwise = mMore
    shiftedRest | Just (_,stationary') <- mExtreme
                = shift dir edge mRest stationary'
                | otherwise
                = S.empty

roll :: Direction -> Platform -> Platform
roll dir (border, mobile, stationary) =
  let fixed | dir `elem` [North, South] = snd
            | otherwise = fst
      variable | dir `elem` [North, South] = fst
               | otherwise = snd
      rolled = map (\(ms,ss) ->
                     let edge | dir `elem` [North, West] = -1
                              | dir == East = snd border
                              | dir == South = fst border
                         shifted = shift dir
                                         edge
                                         (S.map variable ms)
                                         (S.map variable ss)
                         addFixed | dir `elem` [North, South]
                                  = flip (,) . fixed . S.findMin $ ms
                                  | otherwise
                                  = (,) . fixed . S.findMin $ ms
                      in S.map addFixed shifted
                   )
             . filter (not . null . fst)
             $ zip (groupByIndex fixed S.partition mobile)
                   (groupByIndex fixed S.partition stationary)
   in (border, mconcat rolled, stationary)

load :: Platform -> Int
load ((southEdge, _), mobile, _) =
  foldr (\(y,_) l -> southEdge - y + l)
        0
        mobile

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = load . roll North . mkPlatform <$> input
  printAnswer "Total load on north support beams: " answer

spinCycle :: Platform -> Platform
spinCycle platform = foldr (\r more p -> more (r p))
                           id
                           (map roll [North, West, South, East])
                           platform

spinTimes :: Int -> Platform -> Platform
spinTimes cycles platform =
  spinForever !! (startUp + (cycles - startUp) `rem` period)
  where
    spinForever = iterate spinCycle platform
    (startUp, period) = foldr (\p more seen cycle ->
                                let r | Just n <- seen M.!? p
                                      = (n - 1, cycle - n)
                                      | otherwise
                                      = more (M.insert p cycle seen) (cycle + 1)
                                 in r
                              )
                              (error "Spinning forever somehow stopped")
                              spinForever
                              M.empty
                              0

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = load . spinTimes 1000000000 . mkPlatform <$> input
  printAnswer "Load after 1000000000 spin cycles: " answer

main :: IO ()
main = do
  let day = "Day 14: Parabolic Reflector Dish"
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
