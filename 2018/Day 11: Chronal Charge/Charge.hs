#! /usr/bin/env nix-shell
#! nix-shell -i runghc
#! nix-shell -p "haskellPackages.ghcWithPackages (ps: [ ps.containers ])"

module Charge where

import Data.List (intersperse, tails, maximumBy)

powerLevel :: Int -> Int -> Int -> Int
powerLevel gSN x y = hundredsOfPowerLevel - 5
  where
    rackID = x + 10
    basePowerLevel = rackID * y
    rackPowerLevel = basePowerLevel + gSN
    augmentedPowerLevel = rackPowerLevel * rackID
    hundredsOfPowerLevel = (augmentedPowerLevel `mod` 1000) `div` 100

grid :: Int -> [[Int]]
grid gSN = do
  y <- [300,299..1]
  return $ do
    x <- [300,299..1]
    return $ powerLevel gSN x y

showGrid :: [[Int]] -> String
showGrid grid =
  concat [concat (intersperse " "
            [if lvl < 0 then show lvl else ' ':show lvl | lvl <- row] ++ ["\n"])
          | row <- grid]

sumTriples :: [Int] -> [Int] -> [Int] -> [Int]
sumTriples = zipWith3 (\a b c -> a + b + c)

maxX :: Int -> ((Int, Int), (Int, Int), Int) -> ((Int, Int), (Int, Int), Int)
maxX a ((x, y), (xt, yt), tot)
  | a > tot = ((x + 1, y), (x+1, y), a)
  | otherwise = ((x + 1, y), (xt, yt), tot)

maxCoord :: [Int] -> ((Int, Int), (Int, Int), Int)
         -> ((Int, Int), (Int, Int), Int)
maxCoord xs ((_, y), (xt, yt), tot) = foldr maxX ((0,y + 1), (xt, yt), tot) xs

sumRows :: [[Int]] -> [Int]
sumRows = foldr (zipWith (+)) (repeat 0)

sumCols :: Int -> [Int] -> [Int]
sumCols sqSize xs = [sum . take sqSize . drop n $ xs
                    | n <- [0..length xs - sqSize]]

gridLevels :: [[Int]] -> Int -> [[Int]]
gridLevels grd sqSize = map sumRows (takeWhile ((== sqSize) . length)
  (map (take sqSize) (tails (map (sumCols sqSize) grd))))

optimalCoordPower :: [[Int]] -> ((Int, Int), Int)
optimalCoordPower xs =
  twoNthree $ foldr maxCoord ((0, 0), (1, 1), head $ head xs) xs
  where twoNthree (a, b, c) = (b, c)

largestPowerSquare3 :: [[Int]] -> (Int, Int)
largestPowerSquare3 grd = fst $ optimalCoordPower (gridLevels grd 3)

largestPowerSquare :: [[Int]] -> ((Int, Int), Int)
largestPowerSquare grd = fst $ maximumBy compareCoordPower
  (map (\sqSize -> case optimalCoordPower (gridLevels grd sqSize) of
                     (xy,p) -> ((xy,sqSize),p))
       [1..300])
  where compareCoordPower (_,p1) (_,p2) = p1 `compare` p2

main :: IO ()
main = do
  grd <- grid . (read :: String -> Int) <$> readFile "./input"
  report "Largest total power coordinate (3x3): "
         ((\(x,y) -> show x <> (',':show y)) . largestPowerSquare3)
         grd
  report "Largest total power identifier (NxN): " ((\((x,y),p) ->
             show x <> (',':show y <> (',':show p))) . largestPowerSquare)
         grd
  where
    report msg f = putStrLn . (msg <>) . f
