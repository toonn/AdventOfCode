#! /usr/bin/env nix-shell
#! nix-shell -i runghc
#! nix-shell -p "haskellPackages.ghcWithPackages (ps: with ps; [ containers ])"

module MonitoringStation where

import Control.Monad (guard)
import Data.List (intercalate, sortOn)
import qualified Data.Map as M
import qualified Data.Set as S

type Asteroid = (Int, Int)
type Bounds = Asteroid
type Direction = Asteroid
type Asteroids = (Bounds, S.Set Asteroid)

directions :: Bounds -> Asteroid -> [Direction]
directions (bX,bY) (x,y) = do
  let im = negate x
  let iM = bX - x
  let jm = negate y
  let jM = bY - y
  let bI = max (abs im) iM
  let bJ = max (abs jm) jM
  let furthest = bI + bJ
  d <- [1..furthest]
  i <- [0..bI]
  let j = d - i
  guard (0 <= j && j <= bJ)
  guard ((i == 0 && j == 1) || (i == 1 && j == 0) || gcd i j == 1)
  -- guard (not (i > 1 && j `mod` i == 0))
  -- guard (not (j > 1 && i `mod` j == 0))
  sI <- take (i + 1) [id, negate]
  sJ <- take (j + 1) [id, negate]
  let i' = sI i
  let j' = sJ j
  guard (im <= i' && i' <= iM && jm <= j' && j' <= jM)
  return (sI i, sJ j)

increment :: Asteroid -> Direction -> Asteroid
increment (aX, aY) (dX, dY) = (aX + dX, aY + dY)

inBounds :: Asteroid -> Bounds -> Bool
inBounds (aX, aY) (bX, bY) = 0 <= aX && aX <= bX && 0 <= aY && aY <= bY

detect :: Asteroids -> Asteroid -> Direction -> Maybe Asteroid
detect (bounds, as) a d | not (a' `inBounds` bounds) = Nothing
                        | a' `S.member` as = Just a'
                        | otherwise = detect (bounds, as) a' d
  where
    a' = increment a d

detectable :: Asteroids -> Asteroid -> [Asteroid]
detectable (bounds, as) a =
  foldr (\d detected -> case detect (bounds, as) a d of
          Nothing -> detected
          Just a' -> a':detected
        )
        []
        (directions bounds a)

detectables :: Asteroids -> M.Map Asteroid [Asteroid]
detectables (bounds, as) =
  S.foldr (\a -> M.insert a (detectable (bounds, as) a)) M.empty as

maximumDetectable :: M.Map Asteroid [Asteroid] -> (Asteroid, Int)
maximumDetectable = M.foldrWithKey (\k a (k', a') ->
                                     if a > a'
                                       then (k, a)
                                       else (k', a')
                                   )
                                   ((0,0), 0)
                  . M.map length

angle :: Asteroid -> Asteroid -> Double
angle (x,y) (a,b) = atan (negate (x' / y')) + offset
  where
    x' = fromIntegral $ a - x
    y' = fromIntegral $ (b - y)
    offset | y' >= 0 = pi
           | x' < 0 = 2*pi
           | otherwise = 0

vaporizeOrder :: Asteroid -> [Asteroid] -> [Asteroid]
vaporizeOrder center as = sortOn (angle center) as

vaporized200 :: Int -> Asteroids -> Asteroid -> Asteroid
vaporized200 t (bounds, as) a = case detectable (bounds, as) a of
  [] -> error "No 200th asteroid to vaporize."
  ds | length ds + t < 200
       -> vaporized200 (length ds + t) (bounds, as S.\\ S.fromList ds) a
     | otherwise -> vaporizeOrder a ds !! (200 - t - 1)


bet200 :: Asteroids -> Asteroid -> Int
bet200 as a = (\(x,y) -> (100 * x) + y) (vaporized200 0 as a)

parseAsteroids :: String -> Asteroids
parseAsteroids s = ((bX rows,bY rows),bs)
  where
    rows = lines s
    bX [] = 0
    bX (r:_) = length r
    bY = length
    bs = foldr (\as more y ->
                 foldr (\a more' x -> let rest = more' (x+1)
                         in if a == '#'
                           then S.insert (x,y) rest
                           else rest
                       )
                       (const (more (y+1)))
                       as
                       0
               )
               (const S.empty)
               rows
               0

main :: IO ()
main = do
  asteroids <- parseAsteroids <$> readFile "input.txt"
  let detectableAsteroids = detectables asteroids
  let (optimalAsteroid, detectableNr) = maximumDetectable detectableAsteroids
  putStrLn . ("Nr of detectable asteroids: " <>) . show $ detectableNr
  putStrLn . ("Bet for 200th asteroid: " <>) . show $
    bet200 asteroids optimalAsteroid

exampleP11 :: (String, Asteroid, Int)
-- .7..7
-- .....
-- 67775
-- ....7
-- ...87
exampleP11 = ( intercalate "\n" [ ".#..#"
                                , "....."
                                , "#####"
                                , "....#"
                                , "...##"
                                ]
             , (3,4)
             , 8
             )

exampleP12 :: (String, Asteroid, Int)
exampleP12 = ( intercalate "\n" [ "......#.#."
                                , "#..#.#...."
                                , "..#######."
                                , ".#.#.###.."
                                , ".#..#....."
                                , "..#....#.#"
                                , "#..#....#."
                                , ".##.#..###"
                                , "##...#..#."
                                , ".#....####"
                                ]
             , (5,8)
             , 33
             )

exampleP13 :: (String, Asteroid, Int)
exampleP13 = ( intercalate "\n" [ "#.#...#.#."
                                , ".###....#."
                                , ".#....#..."
                                , "##.#.#.#.#"
                                , "....#.#.#."
                                , ".##..###.#"
                                , "..#...##.."
                                , "..##....##"
                                , "......#..."
                                , ".####.###."
                                ]
             , (1,2)
             , 35
             )

exampleP14 :: (String, Asteroid, Int)
exampleP14 = ( intercalate "\n" [ ".#..#..###"
                                , "####.###.#"
                                , "....###.#."
                                , "..###.##.#"
                                , "##.##.#.#."
                                , "....###..#"
                                , "..#.#..#.#"
                                , "#..#.#.###"
                                , ".##...##.#"
                                , ".....#.#.."
                                ]
             , (6,3)
             , 41
             )

exampleP15 :: (String, Asteroid, Int)
exampleP15 = ( intercalate "\n" [ ".#..##.###...#######"
                                , "##.############..##."
                                , ".#.######.########.#"
                                , ".###.#######.####.#."
                                , "#####.##.#.##.###.##"
                                , "..#####..#.#########"
                                , "####################"
                                , "#.####....###.#.#.##"
                                , "##.#################"
                                , "#####.##.###..####.."
                                , "..######..##.#######"
                                , "####.##.####...##..#"
                                , ".#####..#.######.###"
                                , "##...#.##########..."
                                , "#.##########.#######"
                                , ".####.#.###.###.#.##"
                                , "....##.##.###..#####"
                                , ".#.#.###########.###"
                                , "#.#.#.#####.####.###"
                                , "###.##.####.##.#..##"
                                ]
             , (11,13)
             , 210
             )
