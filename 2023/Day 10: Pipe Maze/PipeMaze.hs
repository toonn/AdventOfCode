module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AoC

import Data.List (partition)
import qualified Data.Map as M
import qualified Data.Set as S

type YX = (Int, Int)

data SemiInt = I Int | S Int -- S n = n + 0.5
  deriving (Eq, Show)

instance Ord SemiInt where
  compare (I a) (S b) | a == b = LT
  compare (S a) (I b) | a == b = GT
  compare l r = compare (case l of
                           I a -> a
                           S a -> a
                        )
                        (case r of
                           I b -> b
                           S b -> b
                        )

instance Enum SemiInt where
  toEnum i | (q,r) <- i `quotRem` 2 = let si | r == 0 = I q
                                             | otherwise = S q
                                        in si

  fromEnum (I i) = i * 2
  fromEnum (S i) = i * 2 + 1

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

semicontinue :: [YX] -> S.Set (SemiInt, SemiInt)
semicontinue loop = S.fromList
                  $ map (\(y,x) -> (I y, I x)) loop
                <> zipWith (\(y1,x1) (y2,x2) ->
                             let semiY | y1 < y2   = S y1
                                       | y1 > y2   = S y2
                                       | otherwise = I y1
                                 semiX | x1 < x2   = S x1
                                       | x1 > x2   = S x2
                                       | otherwise = I x1
                              in (semiY, semiX)
                           )
                           loop
                           (tail loop <> take 1 loop)

semiGrid :: M.Map YX (S.Set YX) -> M.Map (SemiInt, SemiInt) (S.Set YX)
semiGrid field =
  let field' = M.mapKeys (\(y,x) -> (I y, I x)) field
      shiftedDown = field'
                 <> ( M.fromSet (const S.empty)
                    . S.map (\(I y,x) -> (S y, x))
                    . M.keysSet
                    $ field'
                    )
      shiftedRight = shiftedDown
                  <> ( M.fromSet (const S.empty)
                     . S.map (\(y,I x) -> (y, S x))
                     . M.keysSet
                     $ shiftedDown
                     )
      shiftedUp = shiftedRight
               <> ( M.fromSet (const S.empty)
                  . S.map (\(I y,x) -> (S (y - 1), x))
                  . M.keysSet
                  . M.filterWithKey (\(y, _) _ -> y == I 0)
                  $ shiftedRight
                  )
      semiField = shiftedUp
               <> ( M.fromSet (const S.empty)
                  . S.map (\(y,I x) -> (y, S (x - 1)))
                  . M.keysSet
                  . M.filterWithKey (\(_, x) _ -> x == I 0)
                  $ shiftedUp
                  )
   in semiField

-- Very inefficient implementation left here for posterity
--
-- semiDistance :: SemiInt -> SemiInt -> SemiInt
-- semiDistance (I a) (I b) = I (abs (a - b))
-- semiDistance (S a) (I b) | a < b = S (b - a - 1)
--                          | otherwise = S (a - b)
-- semiDistance (I a) (S b) | a > b = S (a - b - 1)
--                          | otherwise = S (b - a)
-- semiDistance (S a) (S b) = I (abs (a - b))
--
-- distance :: (SemiInt, SemiInt) -> (SemiInt, SemiInt) -> SemiInt
-- distance (sY1, sX1) (sY2, sX2) = let dy = semiDistance sY1 sY2
--                                      dx = semiDistance sX1 sX2
--                                      d | I y <- dy, I x <- dx = I (y + x)
--                                        | I y <- dy, S x <- dx = S (y + x)
--                                        | S y <- dy, I x <- dx = S (y + x)
--                                        | S y <- dy, S x <- dx = I (y + x + 1)
--                                   in d
--
-- adjacent :: S.Set (SemiInt, SemiInt) -> S.Set (SemiInt, SemiInt) -> Bool
-- adjacent s1 s2 = S.foldr (\sYX1 more _ ->
--                            let r | S.foldr (\sYX2 more _ ->
--                                              let r | distance sYX1 sYX2 == S 0
--                                                    = True
--                                                    | otherwise
--                                                    = more ()
--                                               in r
--                                            )
--                                            (const False)
--                                            s2
--                                            ()
--                                  = True
--                                  | otherwise
--                                  = more ()
--                             in r
--                          )
--                          (const False)
--                          s1
--                          ()
--
-- mergeFirstAdjacent :: S.Set (SemiInt, SemiInt) -> [S.Set (SemiInt, SemiInt)]
--                    -> (Bool, [S.Set (SemiInt, SemiInt)])
-- mergeFirstAdjacent group [] = (False, [])
-- mergeFirstAdjacent group (g:groups) | adjacent group g
--                                     = (True, group <> g : groups)
--                                     | otherwise
--                                     , (b, groups') <- mergeFirstAdjacent group
--                                                                          groups
--                                     = (b, g : groups')
--
-- mergeAdjacent :: [S.Set (SemiInt, SemiInt)] -> [S.Set (SemiInt, SemiInt)]
-- mergeAdjacent [] = []
-- mergeAdjacent (group : groups) =
--   let (merged, groups') = mergeFirstAdjacent group groups
--       contiguousGroups | merged = mergeAdjacent groups'
--                        | otherwise = group : mergeAdjacent groups'
--    in contiguousGroups

semiPred :: SemiInt -> SemiInt
semiPred (I x) = S (x - 1)
semiPred (S x) = I x

semiSucc :: SemiInt -> SemiInt
semiSucc (I x) = S x
semiSucc (S x) = I (x + 1)

sortIntoBins :: [(SemiInt, SemiInt)] -> [S.Set (SemiInt, SemiInt)]
sortIntoBins ps = foldr (\p more bins ->
                          let pNs | (y, x) <- p
                                  = S.fromAscList [ (semiPred y, x)
                                                  , (y, semiPred x)
                                                  , (y, semiSucc x)
                                                  , (semiSucc y, x)
                                                  ]
                              (nonAdjacentBins, adjacentBins) =
                                partition (S.null . S.intersection pNs) bins
                              adjacentBin | null adjacentBins
                                          = S.singleton p
                                          | otherwise
                                          = S.insert p (mconcat adjacentBins)
                           in more (adjacentBin : nonAdjacentBins)
                        )
                        id
                        ps
                        []

contiguous :: S.Set (SemiInt, SemiInt) -> [S.Set (SemiInt, SemiInt)]
contiguous ps = sortIntoBins
              . S.toList
              $ ps

enclosedTiles :: (YX, M.Map YX (S.Set YX)) -> S.Set (SemiInt, SemiInt)
enclosedTiles (start, field) =
  let loop = start : follow field start (S.findMin (field M.! start))
      semicontinuousLoop = semicontinue loop
      semiField = semiGrid field
      offLoop = M.withoutKeys semiField semicontinuousLoop
      contiguousRegions = contiguous . M.keysSet $ offLoop
      ((minY, minX), (maxY,maxX)) | let ks = M.keysSet semiField
                                  = (S.findMin ks, S.findMax ks)
      border = mconcat
             . map S.fromAscList
             $ [ [(minY, x) | x <- [minX..maxX]]
               , [(maxY, x) | x <- [minX..maxX]]
               , [(y, minX) | y <- [minY..maxY]]
               , [(y, maxX) | y <- [minY..maxY]]
               ]

      enclosed = S.filter (\(sy, sx) -> let whole | I _ <- sy, I _ <- sx = True
                                                  | otherwise = False
                                         in whole
                          )
               . mconcat
               . filter ((== 0) . S.size . S.intersection border)
               $ contiguousRegions
   in enclosed

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = S.size . enclosedTiles . mkField <$> input
  printAnswer "Tiles enclosed by the loop: " answer

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
