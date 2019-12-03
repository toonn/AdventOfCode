#! /usr/bin/env nix-shell
#! nix-shell -i runghc
#! nix-shell -p "haskellPackages.ghcWithPackages (ps: with ps; [ containers ])"

module FMS where

import qualified Data.Map as M
import Text.ParserCombinators.ReadP
import Data.List (foldl')

type Coord = Int
type Point = (Coord, Coord)
type Distance = Int
type Steps = Int

type Segment = (Coord, Coord)
type Horizontal = M.Map Coord [Segment]
type Vertical = Horizontal
type Segments = (Horizontal, Vertical)

data Stretch = Up Distance
             | Ri Distance
             | Dn Distance
             | Le Distance
             deriving (Show)

type Wire = [Stretch]

type Intersections = [Point]

manhattan :: Point -> Point -> Distance
manhattan (x1,y1) (x2,y2) = (abs (x1 - x2)) + (abs (y1 - y2))

otherEnd :: Point -> Stretch -> Point
otherEnd (x,y) (Up d) = (x    , y + d)
otherEnd (x,y) (Ri d) = (x + d, y)
otherEnd (x,y) (Dn d) = (x    , y - d)
otherEnd (x,y) (Le d) = (x - d, y)

insert :: Point -> Point -> Segments -> Segments
insert (x1,y1) (x2,y2) (horizontal, vertical)
  | x1 == x2 = (horizontal, M.alter (include (ym, yM)) x1 vertical)
  | y1 == y2 = (M.alter (include (xm, xM)) y1 horizontal, vertical)
  where
    xm = min x1 x2
    xM = max x1 x2
    ym = min y1 y2
    yM = max y1 y2
    include v Nothing = Just [v]
    include v (Just vs) = Just (v:vs)

empty :: Segments
empty = (M.empty, M.empty)

segments' :: Point -> Wire -> Segments
segments' _ [] = empty
segments' o (s:p) = insert (o' o s) oE $ segments' oE p
  where
    oE = otherEnd o s
    o' (0,0) (Up _) = (0,1)
    o' (0,0) (Ri _) = (1,0)
    o' (0,0) (Dn _) = (0,-1)
    o' (0,0) (Le _) = (-1,0)
    o' o _ = o

segments :: Wire -> Segments
segments = segments' (0,0)

wire :: String -> Wire
wire = fst . last . readP_to_S
  (sepBy (   (char 'U' >> digits >>= (return . Up . read))
         +++ (char 'R' >> digits >>= (return . Ri . read))
         +++ (char 'D' >> digits >>= (return . Dn . read))
         +++ (char 'L' >> digits >>= (return . Le . read))
         )
         (char ',')
  )
  where
    digits = munch (`elem` "0123456789")

followStretch :: Segments -> Point -> Point -> Intersections -> Intersections
followStretch (horizontal, vertical) (x1,y1) (x2,y2) intersections
  | x1 == x2 = M.foldrWithKey (intersect x1 ym yM) intersections horizontal
  | y1 == y2 = M.foldrWithKey (intersect y1 xm xM) intersections vertical
  where
    xm = min x1 x2
    xM = max x1 x2
    ym = min y1 y2
    yM = max y1 y2

    inInterval a (b,c) = b <= a && a <= c

    twist (a,b) | y1 == y2 = (b,a)
                | otherwise = (a,b)

    intersect a m mM k vs intersects
      | k `inInterval` (m, mM), any (a `inInterval`) vs =
        (twist (a,k)):intersects
      | otherwise = intersects

follow :: Segments -> Point -> Wire -> Intersections
follow _ _ [] = []
follow segs o (s:p) = intersections (follow segs oE p)
  where
    oE = otherEnd o s
    intersections = followStretch segs o oE

closest :: Intersections -> Distance
closest = minimum . map (manhattan (0,0))

steps' :: Intersections
       -> (Point, Steps, M.Map Point Steps)
       -> Stretch
       -> (Point, Steps, M.Map Point Steps)
steps' is ((x,y), stps, s) stretch = (o' d, stps + d, s')
  where
    d = case stretch of
      (Up a) -> a
      (Ri a) -> a
      (Dn a) -> a
      (Le a) -> a

    o' c = case stretch of
      (Up _) -> (x, y + c)
      (Ri _) -> (x + c, y)
      (Dn _) -> (x, y - c)
      (Le _) -> (x - c, y)

    checkAndStore c m | (o' c) `M.member` m = m
                      | (o' c) `elem` is = M.insert (o' c) (stps + c) m
                      | otherwise = m

    s' = foldr checkAndStore s [1..d]

steps :: Wire -> Intersections -> M.Map Point Steps
steps w is = (\(a,b,c) -> c) $ foldl' (steps' is) ((0,0), 0, M.empty) w

minimalDelay :: Wire -> Wire -> Intersections -> Steps
minimalDelay w1 w2 is =
  minimum . M.elems $ M.unionWith (+) (steps w1 is) (steps w2 is)

main :: IO ()
main = do
  [one, two] <- lines <$> readFile "input.txt"
  let inters = follow (segments (wire one)) (0,0) (wire two)
  let dist = closest inters
  putStrLn . ("Distance to closest intersection: " <>) . show $ dist
  let stps = minimalDelay (wire one) (wire two) inters
  putStrLn . ("Total steps to minimally delayed intersection: " <>) . show $
    stps

example1, example2, example3 :: (String, Distance, Steps)
example1 = ("R8,U5,L5,D3\n\
            \U7,R6,D4,L4"
           , 3 + 3
           , 15 + 15
           )

example2 = ("R75,D30,R83,U83,L12,D49,R71,U7,L72\n\
            \U62,R66,U55,R34,D71,R55,D58,R83"
           , 159
           , 610
           )

example3 = ("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\n\
            \U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
           , 135
           , 410
           )
