module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AoC

import qualified Data.Map as M
import qualified Data.Set as S

type YX = (Int, Int)

type Input = [[S.Set YX]]

pipe :: Parser (S.Set YX)
pipe = do
  p <- oneOf "|-LJ7F.S"
  pure $ case p of
    '|' -> S.fromAscList [(-1,0), (1,0)]
    '-' -> S.fromAscList [(0,-1), (0,1)]
    'L' -> S.fromAscList [(-1,0), (0,1)]
    'J' -> S.fromAscList [(-1,0), (0,-1)]
    '7' -> S.fromAscList [(0,-1), (1,0)]
    'F' -> S.fromAscList [(0,1), (1,0)]
    '.' -> S.empty
    'S' -> S.fromAscList [(y,x) | y <- [-1..1], x <- [-1..1], abs y /= abs x]

row :: Parser [S.Set YX]
row = some pipe

parser :: Parser Input
parser = sepEndBy1 row eol <* eof

mkField :: Input -> (YX, M.Map YX (S.Set YX))
mkField tiles =
  let tiles' = M.fromAscList
             . foldr (\r more y ->
                       foldr (\ns more x ->
                               ((y,x), S.map (\(dy,dx) -> (y+dy,x+dx)) ns)
                               : more (x + 1)
                             )
                             (const [])
                             r
                             0
                       <> more (y + 1)
                     )
                     (const [])
                     tiles
             $ 0
   in M.mapAccumWithKey
        (\s yx ns ->
          let ns' | S.size ns == 4
                  = ( yx
                    , S.filter (\n ->
                                 S.member yx
                                          (M.findWithDefault S.empty n tiles')
                               )
                               ns
                    )
                  | otherwise
                  = (s, ns)
           in ns'
        )
        (-1,-1)
        tiles'

follow :: M.Map YX (S.Set YX) -> YX -> YX -> [YX]
follow field from at = at : go from at
  where
    go :: YX -> YX -> [YX]
    go f a = let to = S.findMin . S.filter (/= f)
                    $ field M.! a
                 path | to == from = []
                      | otherwise  = to : go a to
              in path

farthestDistance :: (YX, M.Map YX (S.Set YX)) -> Int
farthestDistance (start, field) =
  let [n1, n2] = S.toList (field M.! start)
      paths = zip (follow field start n1) (follow field start n2)
   in foldr (\(p1,p2) more d ->
              let d' | p1 == p2 = d
                     | otherwise = more (d + 1)
               in d'
            )
            (const (-1))
            paths
            1

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = farthestDistance . mkField <$> input
  printAnswer "Distance to farthest point: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 10: Pipe Maze"
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
