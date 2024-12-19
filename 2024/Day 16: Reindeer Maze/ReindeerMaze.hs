module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S

data Orientation = N | E | S | W deriving (Show, Eq, Ord)

type POGG = (YX, Orientation)

type Input = [[Char]]

parser :: Parser Input
parser = characterGrid <* eof

move :: Orientation -> YX -> YX
move N (y,x) = (y - 1, x)
move E (y,x) = (y, x + 1)
move S (y,x) = (y + 1, x)
move W (y,x) = (y, x - 1)

turnRight :: Orientation -> Orientation
turnRight N = E
turnRight E = S
turnRight S = W
turnRight W = N

turnLeft :: Orientation -> Orientation
turnLeft N = W
turnLeft E = N
turnLeft S = E
turnLeft W = S

neighbors :: M.Map YX Char -> YX -> POGG -> [POGG]
neighbors grid g (yx, o)
  = let step o' | let yx' = move o' yx, Just t <- grid M.!? yx', t `elem` ".ES"
                = Just (yx', o')
                | otherwise = Nothing
     in mapMaybe step $ [o, turnLeft o, turnRight o]

turns :: Orientation -> Orientation -> Int
turns o o' | o == o' = 0
           | o == turnRight o' || o == turnLeft o' = 1
           | otherwise = 2

distance :: POGG -> POGG -> Int
distance (yx, o) (yx', o') = manhattan yx yx' + 1000 * turns o o'

heuristic :: YX -> POGG -> Int
heuristic g (yx, o) = manhattan yx g + 1000 * ( if fst yx == fst g
                                                || snd yx == snd g
                                                then 0
                                                else 1
                                              )

isGoal :: YX -> POGG -> Bool
isGoal = (. fst) . (==)

findKey :: Eq a => a -> M.Map k a -> k
findKey v = (\(Just (k,_)) -> k) . find ((== v) . snd) . M.assocs

shortestPath :: YX -> YX -> M.Map YX Char -> Int
shortestPath start end grid = aStar (neighbors grid end)
                                    distance
                                    (heuristic end)
                                    (isGoal end)
                                    (start, E)

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = (\grid ->
                 let start = findKey 'S' grid
                     end = findKey 'E' grid
                  in shortestPath start end grid
               )
             . foldYX
           <$> input
  printAnswer "Lowest possible score: " answer

neighborsSlow :: M.Map YX Char -> POGG -> [POGG]
neighborsSlow grid (yx,o) = let yx' = move o yx
                                step | Just t <- grid M.!? yx', t `elem` ".ES"
                                     = ((yx',o):)
                                     | otherwise = id
                             in step [ (yx, turnLeft o), (yx, turnRight o) ]

partitionMin :: M.Map POGG (Int, S.Set YX) -> S.Set POGG
             -> (S.Set POGG, S.Set POGG)
partitionMin costs yxos
  = foldr (\yxo more (mins,nins) mMin ->
            let Just c = costs M.!? yxo
                newMin = more (S.singleton yxo, mins <> nins) (Just c)
                res | Just m <- mMin
                    = case c `compare` m of
                        LT -> newMin
                        EQ -> more (S.insert yxo mins, nins) mMin
                        GT -> more (mins, S.insert yxo nins) mMin
                    | otherwise = newMin
             in res
          )
          const
          yxos
          (mempty, mempty)
          Nothing

mirror :: POGG -> POGG
mirror (yx,o) | N <- o = (yx, S)
              | E <- o = (yx, W)
              | S <- o = (yx, N)
              | W <- o = (yx, E)

connected :: S.Set POGG -> S.Set POGG -> S.Set (POGG,POGG)
connected set = foldr (\yxo ->
                        let yxo' = mirror yxo
                            ins | yxo' `elem` set = S.insert (yxo',yxo)
                                | otherwise = id
                         in ins
                      )
                      mempty

bidirAllPaths :: M.Map YX Char
              -> M.Map POGG (Int, S.Set YX)
              -> S.Set POGG
              -> S.Set POGG
              -> Maybe Int
              -> S.Set YX
bidirAllPaths grid seen stepSet compSet shortestPath
  | null stepSet || null compSet = mempty
  | otherwise
  = let (candSet, nandSet) = partitionMin seen stepSet
        paths = connected candSet compSet
        (shortestPath', partOfPath) = foldr (\(yxo1,yxo2) more (mSP, vs) ->
                                              let (c1,vs1) = seen M.! yxo1
                                                  (c2,vs2) = seen M.! yxo2
                                                  c12 = c1 + c2
                                                  vs12 = vs1 <> vs2
                                                  r12 = (Just c12, vs <> vs12)
                                                  res | Just sP <- mSP
                                                      , c12 == sP
                                                      = r12
                                                      | Nothing <- mSP
                                                      = r12
                                                      | otherwise = (mSP, vs)
                                               in more res
                                            )
                                            id
                                            paths
                                            (shortestPath, mempty)
        toStep = candSet S.\\ (S.map fst paths)
        compSet' = compSet S.\\ (S.map snd paths)
        (seen', stepped)
          = foldr
              (\yxo more (s, st) ->
                let Just (c, vs) = s M.!? yxo
                 in more
                  $ foldr
                      (\yxo' more (s, st) ->
                        let Just (sc, svs) = s M.!? yxo'
                            c' = c + distance yxo yxo'
                            n | M.notMember yxo' s || c' < sc
                              = ( M.insert yxo' (c', S.insert (fst yxo') vs) s
                                , S.insert yxo' st
                                )
                              | c' == sc = (M.insert yxo' (sc, vs <> svs) s, st)
                              | otherwise = (s, st)
                         in more n
                      )
                      id
                      (neighborsSlow grid yxo)
                      (s,st)
              )
              id
              toStep
              (seen, mempty)
        minC = S.findMin . S.map (fst . (seen' M.!)) $ compSet'
        stepped' | Just sP <- shortestPath'
                 = S.filter ((<= sP) . (+ minC) . fst . (seen' M.!)) stepped
                 | otherwise = stepped
     in partOfPath
     <> bidirAllPaths grid seen' compSet' (stepped' <> nandSet) shortestPath'

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = length
             . (\grid ->
                 let start = (findKey 'S' grid, E)
                     end = findKey 'E' grid
                     seen = M.fromList ( (start, (0, S.singleton (fst start)))
                                       : map (\o -> ( (end,o)
                                                    , (0, S.singleton end))
                                             )
                                             [N,E,S,W]
                                       )
                  in bidirAllPaths grid
                                   seen
                                   (S.singleton start)
                                   ( S.fromList . map (\o -> (end,o))
                                   $ [N,E,S,W]
                                   )
                                   Nothing
               )
             . foldYX
           <$> input
  printAnswer "Tiles on any shortest Path: " answer

main :: IO ()
main = do
  let day = "Day 16: Reindeer Maze"
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
