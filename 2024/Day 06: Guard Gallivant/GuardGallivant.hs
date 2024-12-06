module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import qualified Data.Map as M
import qualified Data.Set as S

data Orientation = N | E | S | W deriving (Show, Eq, Ord)

type Input = [[Char]]

parser :: Parser Input
parser = characterGrid <* eof

step :: YX -> Orientation -> YX
step (y,x) N = (y - 1, x)
step (y,x) E = (y, x + 1)
step (y,x) S = (y + 1, x)
step (y,x) W = (y, x - 1)

turn :: Orientation -> Orientation
turn N = E
turn E = S
turn S = W
turn W = N

patrol' :: M.Map YX Char -> [(YX,Orientation)] -> YX -> Orientation
        -> [(YX,Orientation)]
patrol' layout seen p o =
  let next = step p o
      spot = M.findWithDefault ' ' next layout
      res | ' ' <- spot = seen
          | '#' <- spot, let o' = turn o = patrol' layout ((p,o') : seen) p o'
          | otherwise = patrol' layout ((next,o) : seen) next o
   in res


patrol :: M.Map YX Char -> [(YX,Orientation)]
patrol layout =
  let start = M.foldrWithKey (\k c rest -> if c == '^' then k else rest)
                             (error "No starting position")
                             layout
   in reverse (patrol' layout [(start,N)] start N)

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = length . S.fromList . map fst . patrol . foldYX <$> input
  printAnswer "Distinct positions: " answer

isCycle :: M.Map YX Char -> S.Set (YX,Orientation) -> YX -> Orientation -> Bool
isCycle layout seen p o =
  let next = step p o
      spot = M.findWithDefault ' ' next layout
      res | S.member (next,o) seen = True
          | ' ' <- spot = False
          | '#' <- spot, let o' = turn o
          = isCycle layout (S.insert (p,o') seen) p o'
          | otherwise = isCycle layout (S.insert (next,o) seen) next o
   in res

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = length
             . (\layout ->
                 foldr
                   (\(p,o) more seen ->
                     let seen' = S.insert p seen
                         potential = step p o
                         potIns | S.notMember potential seen'
                                , M.findWithDefault ' ' potential layout == '.'
                                , isCycle (M.insert potential '#' layout)
                                          mempty
                                          p
                                          o
                                = S.insert potential
                                | otherwise = id
                      in potIns (more seen')
                   )
                   mempty
                   (patrol layout)
                   mempty
               )
               . foldYX
               <$> input
  printAnswer "Potential obstructions: " answer

main :: IO ()
main = do
  let day = "Day 06: Guard Gallivant"
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
