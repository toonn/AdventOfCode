module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Data.List (sortOn)
import qualified Data.SBV as SBV

type Vector = (Int, Int, Int)
type Hailstone = (Vector, Vector)
type Intersection = (Double, Double)

type SHailstone = ( ( SBV.SInteger
                    , SBV.SInteger
                    , SBV.SInteger
                    )
                  , ( SBV.SInteger
                    , SBV.SInteger
                    , SBV.SInteger
                    )
                  )

type Input = [Hailstone]

vector :: Parser Vector
vector = (,,) <$> signed integer
      <* comma <*> signed integer
      <* comma <*> signed integer
  where
    comma = lexeme (char ',')

parser :: Parser Input
parser = sepEndBy1 ((,) <$> vector <* lexeme (char '@') <*> vector) eol <* eof

-- Based on intersection of two lines based on their equations.
-- This means it does not support lines parallel to the y axis.
-- It does not take into account coincident lines.
-- https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_line_equations
infinite2DIntersection :: ((Int,Int),(Int,Int)) -> ((Int,Int),(Int,Int))
                       -> Maybe Intersection
infinite2DIntersection ((x,y),(vx,vy)) ((x',y'),(vx',vy'))
  | a == b = Nothing
  | otherwise = Just ((d - c)/(a - b),a*(d-c)/(a-b)+c)
  where
    a = fromIntegral vy / fromIntegral vx
    c = fromIntegral y - fromIntegral vy * (fromIntegral x / fromIntegral vx)
    b = fromIntegral vy' / fromIntegral vx'
    d = fromIntegral y'
      - fromIntegral vy' * (fromIntegral x' / fromIntegral vx')

inFuture :: (Int, Int) -> (Int, Int) -> Intersection -> Bool
inFuture (x,y) (vx,vy) (cx,cy) =
  (vx == 0 || ((cx - fromIntegral x) / fromIntegral vx) >= 0)
  && (vy == 0 || ((cy - fromIntegral y) / fromIntegral vy) >= 0)

testIntersections :: Double -> Double -> Input -> Int
testIntersections lower upper hailstones
  = length
  . filter (\(x,y) -> lower <= x && x <= upper && lower <= y && y <= upper)
  $ foldr (\line@((x,y,_),(vx,vy,_)) more lines ->
            foldr (\(((x',y',_),(vx',vy',_))) rest ->
                    let mC = infinite2DIntersection ((x,y),(vx,vy))
                                                    ((x',y'),(vx',vy'))
                        r | Just collision@(cx,cy) <- mC
                          , inFuture (x,y) (vx,vy) collision
                          , inFuture (x',y') (vx',vy') collision
                          = (collision:)
                          | otherwise
                          = id
                     in r rest
                  )
                  (more (line:lines))
                  lines
          )
          (const [])
          hailstones
          []

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = testIntersections 200000000000000 400000000000000 <$> input
  printAnswer "Intersections in test area: " answer

cross :: Vector -> Vector -> Vector
cross (a1,a2,a3) (b1,b2,b3) = ( a2*b3 - a3*b2
                              , a3*b1 - a1*b3
                              , a1*b2 - a2*b1
                              )

planeIntersection :: Vector -> Vector -> Vector -> Vector
planeIntersection a b c = (\(x,y,z) -> let d = gcd x (gcd y z)
                                        in ( x `quot` d
                                           , y `quot` d
                                           , z `quot` d
                                           )
                          )
                        $ (a `cross` b) `cross` (a `cross` c)

-- x1 + t1 vx1 = xs + t1 vxs
-- y1 + t1 vy1 = ys + t1 vys
-- z1 + t1 vz1 = zs + t1 vzs
-- x2 + t2 vx2 = xs + t2 vxs
-- y2 + t2 vy2 = ys + t2 vys
-- z2 + t2 vz2 = zs + t2 vzs
-- x3 + t3 vx3 = xs + t3 vxs
-- y3 + t3 vy3 = ys + t3 vys
-- z3 + t3 vz3 = zs + t3 vzs
--
-- t1 = (x1 - xs) / (vxs - vx1) = (y1 - ys) / (vys - vy1)
-- xs = x1 - (vxs - vx1)(y1 - ys) / (vys - vy1)
-- t2 = (x2 - xs) / (vxs - vx2) = (y2 - ys) / (vys - vy2)
--
-- x1 - (vxs - vx1)(y1 - ys) / (vys - vy1)
-- = x2 - (vxs - vx2)(y2 - ys) / (vys - vy2)
--
-- (x1(vys - vy1) - (vxs - vx1)(y1 - ys))(vys - vy2)
-- = (x2(vys - vy2) - (vxs - vx2)(y2 - ys))(vys - vy1)
--
-- (x1 - x2)(vys - vy1)(vys - vy2)
-- = (vxs - vx1)(y1 - ys)(vys - vy2) - (vxs - vx2)(y2 - ys)(vys - vy1)
--
-- vxs = ((x1 - x2)(vys - vy1)(vys - vy2) + vx1(y1 - ys)(vys - vy2)
--       - vx2(y2 - ys)(vys - vy1))
--       / ((y1 - ys)(vys - vy2) - (y2 - ys)(vys - vy1))
--     = ((x3 - x2)(vys - vy3)(vys - vy2) + vx3(y3 - ys)(vys - vy2)
--       - vx2(y2 - ys)(vys - vy3))
--       / ((y3 - ys)(vys - vy2) - (y2 - ys)(vys - vy3))
--
-- ((x1 - x2)(vys - vy1)(vys - vy2) + vx1(y1 - ys)(vys - vy2)
-- - vx2(y2 - ys)(vys - vy1))
-- * ((y3 - ys)(vys - vy2) - (y2 - ys)(vys - vy3))
-- = ((x3 - x2)(vys - vy3)(vys - vy2) + vx3(y3 - ys)(vys - vy2)
--   - vx2(y2 - ys)(vys - vy3))
--   * ((y1 - ys)(vys - vy2) - (y2 - ys)(vys - vy1))

-- x1 + t1 vx1 = xs + t1 vxs
-- y1 + t1 vy1 = ys + t1 vys
-- z1 + t1 vz1 = zs + t1 vzs
-- x2 + t2 vx2 = xs + t2 vxs
-- y2 + t2 vy2 = ys + t2 vys
-- z2 + t2 vz2 = zs + t2 vzs
-- x3 + t3 vx3 = xs + t3 vxs
-- y3 + t3 vy3 = ys + t3 vys
-- z3 + t3 vz3 = zs + t3 vzs
--
-- x1 + t1 (vx1 - vxs) = xs
-- x2 + t2 vx2 = x1 + t1(vx1 - vxs) + t2 vxs
-- x3 + t3 vx3 = x1 + t1(vx1 - vxs) + t3 vxs
--
-- x1 + t1 (vx1 - vxs) = xs
-- (x2 + t2 vx2 - x1 - t1 vx1) / (t2 - t1) = vxs
-- -t1 + (x3 - x1 + t1 vx1)(t2 - t1) / (x2 + t2 vx2 - x1 - t1 vx1) = t3
--
-- y3 - (-t1 + (x3 - x1 + t1 vx1)(t2 - t1) / (x2 + t2 vx2 - x1 - t1 vx1))
--  (vys - vx3) = ys
-- (y2 - y3 + t2 vy2 - (-t1 + (x3 - x1 + t1 vx1)(t2 - t1) / (x2 + t2 vx2 - x1 - t1 vx1)) vx3) / (t2 - (-t1 + (x3 - x1 + t1 vx1)(t2 - t1) / (x2 + t2 vx2 - x1 - t1 vx1))) = vys

-- (x0,y0,z0) (vx0,vy0,vz0)
-- (x1-x0, y1-y0, z1-z0) (vx1 - vx0, vy1 - vy0, vz1 - vz0)
-- x1 - x0 + t1 vx1 - t1 vx0 = xs - x0 + t1 vxs - t1 vx0

-- (v1 x (ps - p1)) x (v2 x (ps - p2)) = l vs
--   vyN (zs - zN) - vzN (ys - yN)
--   vzN (xs - xN) - vxN (zs - zN)
--   vxN (ys - yN) - vyN (xs - xN)
-- (vz1 (xs - x1) - vx1 (zs - z1))(vx2 (ys - y2) - vy2 (xs - x2))
-- - (vx1 (ys - y1) - vy1 (xs - x1))(vz2 (xs - x2) - vx2 (zs - z2)) = l vxs
-- (vx1 (ys - y1) - vy1 (xs - x1))(vy2 (zs - z2) - vz2 (ys - y2))
-- - (vy1 (zs - z1) - vz1 (ys - y1))(vx2 (ys - y2) - vy2 (xs - x2)) = l vys
-- (vy1 (zs - z1) - vz1 (ys - y1))(vz2 (xs - x2) - vx2 (zs - z2))
-- - (vz1 (xs - x1) - vx1 (zs - z1))(vy2 (zs - z2) - vz2 (ys - y2)) = l vzs

-- (p1 - p0) + t1 (v1 - v0) = t1 vs
-- (p2 - p0) + t2 (v2 - v0) = t2 vs
-- n1(x1 - x0) + vx1 = vxs
-- n1(y1 - y0) + vy1 = vys
-- n1(z1 - z0) + vz1 = vzs
-- n2(x2 - x0) + vx2 = vxs
-- n2(y2 - y0) + vy2 = vys
-- n2(z2 - z0) + vz2 = vzs
-- n1 = (n2(x2 - x0) + vx2 - vx1) / (x1 - x0)
-- n1 = (n2(y2 - y0) + vy2 - vy1) / (y1 - y0)
-- n2 = ((vy2 - vy1)(x1 - x0) - (vx2 - vx1)(y1 - y0)) / ((y1 - y0)(x2 - x0) - (x1 - x0)(y2 - y0))
-- (((vy2 - vy1)(x1 - x0) - (vx2 - vx1)(y1 - y0)) / ((y1 - y0)(x2 - x0) - (x1 - x0)(y2 - y0)))(x2 - x0) + vx2 = vxs
-- (((vy2 - vy1)(x1 - x0) - (vx2 - vx1)(y1 - y0)) / ((y1 - y0)(x2 - x0) - (x1 - x0)(y2 - y0)))(y2 - y0) + vy2 = vys
-- (((vy2 - vy1)(x1 - x0) - (vx2 - vx1)(y1 - y0)) / ((y1 - y0)(x2 - x0) - (x1 - x0)(y2 - y0)))(z2 - z0) + vz2 = vzs

-- Gave up here and tried constraint solving instead.
collisionConstraint :: SHailstone
                    -> Hailstone
                    -> SBV.SBool
collisionConstraint projectile@((xs,ys,zs),(vxs,vys,vzs)) ((x,y,z),(vx,vy,vz))
  = do let integralToSymbolic = fromIntegral
           xo = integralToSymbolic x
           yo = integralToSymbolic y
           zo = integralToSymbolic z
           vxo = integralToSymbolic vx
           vyo = integralToSymbolic vy
           vzo = integralToSymbolic vz
       SBV.quantifiedBool $ \(SBV.Exists t) ->
         SBV.sAnd [ (xo + t * vxo) SBV..== (xs + t * vxs)
                  , (yo + t * vyo) SBV..== (ys + t * vys)
                  , (zo + t * vzo) SBV..== (zs + t * vzs)
                  , (t :: SBV.SInteger) SBV..>= 0
                  ]

coordinateSum :: Input -> IO Integer
coordinateSum input = do
  model <- SBV.sat $ do
             xs <- SBV.sInteger "xs"
             ys <- SBV.sInteger "ys"
             zs <- SBV.sInteger "zs"
             vxs <- SBV.sInteger "vxs"
             vys <- SBV.sInteger "vys"
             vzs <- SBV.sInteger "vzs"
             SBV.solve $ map (collisionConstraint ((xs,ys,zs),(vxs,vys,vzs)))
                             input
  let Just xs = SBV.getModelValue "xs" model
  let Just ys = SBV.getModelValue "ys" model
  let Just zs = SBV.getModelValue "zs" model
  pure (xs + ys + zs)

part2 :: Parsed Input -> IO ()
part2 input = do
  answer <- either (pure . Left)
                   -- 6 variables for the start and direction of the projectile
                   -- 1 variable per collision
                   -- 3 collisions provide 9 equations for 9 variables
                   -- As long as no two collisions are in the same location
                   -- that's sufficient to solve the system.
                   -- Including all collisions wastes time finding the time for
                   -- all the other collisions.
                   ((Right <$>) . coordinateSum . take 3)
                   input
  printAnswer "Sum of initial coordinates: " answer

main :: IO ()
main = do
  let day = "Day 24: Never Tell Me The Odds"
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
