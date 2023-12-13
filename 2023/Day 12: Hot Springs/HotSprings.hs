module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AoC

import Control.Arrow (first)
import Data.List (intercalate, tails)
import qualified Data.Map as M

type SpringState = ([Char], [Int])
type BinnedSpringState = ([[Char]], [Int])

type Input = [SpringState]

springState :: Parser SpringState
springState = do
  states <- takeWhile1P (Just "Spring state") (`elem` ".#?")
  hspace
  groupSizes <- sepBy integer (char ',')
  pure (states, groupSizes)

parser :: Parser Input
parser = sepEndBy1 springState eol <* eof

bins :: [Char] -> [[Char]]
bins springs =
  foldr (\spring more g ->
          let next | spring /= '.' || null g = id
                   | otherwise = (g:)
              g' | spring == '.' = []
                 | otherwise = g <> [spring]
           in next (more g')
        )
        (\g -> let l | null g = []
                     | otherwise = [g]
                in l
        )
        springs
        []

arrangements :: M.Map BinnedSpringState Int -> BinnedSpringState
             -> M.Map BinnedSpringState Int
arrangements ways springState | M.member springState ways = ways
arrangements ways ([], []) = M.insert ([], []) 1 ways
arrangements ways ([], groupSizes) = M.insert ([], groupSizes) 0 ways
arrangements ways (bins, [])
  | any (any (== '#')) bins = M.insert (bins, []) 0 ways
  | otherwise = M.insert (bins, []) 1 ways
arrangements ways (bin:bins, size:groupSizes)
  | length bin < size
  = let dropBin | any (== '#') bin = M.insert (bin:bins, size:groupSizes) 0 ways
                | otherwise
                , next <- (bins, size:groupSizes)
                , nWs <- arrangements ways next
                , Just nrNext <- nWs M.!? next
                = M.insert (bin:bins, size:groupSizes) nrNext nWs
     in dropBin
  | otherwise
  = let fitWays | (group,bin') <- splitAt size bin
                , null bin' || head bin' /= '#'
                , rest <- (dropWhile null (drop 1 bin':bins), groupSizes)
                , restWays <- arrangements ways rest
                , Just nrRest <- restWays M.!? rest
                = M.insert (bin:bins, size:groupSizes) nrRest restWays
                | otherwise
                = M.insert (bin:bins, size:groupSizes) 0 ways
        dropSpring | ('?':bin') <- bin
                   , next <- (dropWhile null (bin':bins), size:groupSizes)
                   , nWs <- arrangements fitWays next
                   , Just nrNext <- nWs M.!? next
                   = M.insertWith (+) (bin:bins, size:groupSizes) nrNext nWs
                   | otherwise
                   = fitWays
     in dropSpring

nrArrangements :: BinnedSpringState -> Int
nrArrangements (springs, groupSizes)
  | binnedSpringState <- (springs, groupSizes)
  = arrangements M.empty binnedSpringState M.! binnedSpringState

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . map (nrArrangements . first bins) <$> input
  printAnswer "Number of arrangements: " answer

unfoldRecord :: SpringState -> SpringState
unfoldRecord (springs, groupSizes) = ( intercalate "?" . replicate 5 $ springs
                                     , mconcat . replicate 5 $ groupSizes
                                     )

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = sum . map (nrArrangements . first bins . unfoldRecord) <$> input
  printAnswer "Number of arrangements after unfolding: " answer

main :: IO ()
main = do
  let day = "Day 12: Hot Springs"
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
