module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Data.List (partition)
import qualified Data.Map as M
import qualified Data.Set as S

type VisitedPotentialPlace = (S.Set YX, Int, YX, Int)
type HikingTrails = M.Map YX Char

type Input = [[Char]]

parser :: Parser Input
parser = characterGrid <* eof

cardinals :: YX -> [(Char, YX)]
cardinals (y,x) = [ (contrary, (y + dy, x + dx))
                  | dy <- [-1..1]
                  , dx <- [-1..1]
                  , abs dy /= abs dx
                  , let contrary = (M.fromAscList [ ((-1,0),'v')
                                                  , ((0,-1),'>')
                                                  , ((0, 1),'<')
                                                  , ((1, 0),'^')
                                                  ]
                                   ) M.! (dy,dx)
                  ]

slide :: HikingTrails -> S.Set YX -> YX -> Int -> (S.Set YX, YX, Int)
slide trails visited yx l =
  let ps = filter ((`S.notMember` visited) . snd)
         . filter (\(_,p) -> '#' /= M.findWithDefault '#' p trails)
         $ cardinals yx
      (ds, ps') = partition (\(c,p) -> c == M.findWithDefault '#' p trails)
                            ps
      downs = map snd ds
      vs = S.insert yx visited <> S.fromList downs
      (visited', yx', l') | [(_,next)] <- ps' = slide trails vs next (l + 1)
                          | otherwise = (vs, yx, l + 1)
   in (visited', yx', l')


neighbors :: HikingTrails -> VisitedPotentialPlace -> [VisitedPotentialPlace]
neighbors trails (visited, potentialPath, yx, l) =
  let ns = [ slide trails visited yx' l
           | (contrary, yx') <- cardinals yx
           , S.notMember yx' visited
           , let location = M.findWithDefault '#' yx' trails
           , location /= '#'
           , location /= contrary
           ]
   in map (\(vs', yx', l') ->
            (visited <> vs', potentialPath - length vs', yx', l')
          )
          ns

distance :: VisitedPotentialPlace -> VisitedPotentialPlace -> Int
distance (_, _, _, l1) (_, _, _, l2) = - (abs (l2 - l1))

startYX :: HikingTrails -> YX
startYX = fst . M.findMin . M.filter (== '.')

goalYX :: HikingTrails -> YX
goalYX = fst . M.findMax . M.filter (== '.')

-- Sliding every downslope you don't take and slideUp'ing every upslope you
-- can't take anymore would make the solution a bit more efficient.
heuristic :: YX -> VisitedPotentialPlace -> Int
heuristic goal p@(_, potential, _, _) | isGoal goal p = 0
                                      | otherwise = - potential

isGoal :: YX -> VisitedPotentialPlace -> Bool
isGoal goal (_, _, yx, _) = goal == yx

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = negate
             . (\hikingTrails ->
                 let start = startYX hikingTrails
                     goal = goalYX hikingTrails
                     potentialPath = length (M.filter (/= '#') hikingTrails)
                  in aStar (neighbors hikingTrails)
                           distance
                           (heuristic goal)
                           (isGoal goal)
                           (S.singleton start, potentialPath, start, 0)
               )
             . foldYX
           <$> input
  printAnswer "Steps in the longest hike: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 23: A Long Walk"
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
