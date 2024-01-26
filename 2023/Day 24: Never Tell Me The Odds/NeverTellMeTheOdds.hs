module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

type Vector = (Int, Int, Int)
type Hailstone = (Vector, Vector)
type Intersection = (Double, Double)

type Input = [Hailstone]

vector :: Parser Vector
vector = (,,) <$> signed integer
      <* comma <*> signed integer
      <* comma <*> signed integer
  where
    comma = lexeme (char ',')

parser :: Parser Input
parser = sepEndBy1 ((,) <$> vector <* lexeme (char '@') <*> vector) eol <* eof

-- -- Based on intersection of two lines based on two points on each line.
-- -- https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line
-- infinite2DIntersection :: ((Int, Int),(Int, Int)) -> ((Int, Int),(Int, Int))
--                        -> Maybe Intersection
-- infinite2DIntersection ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4))
--   | denominator == 0 = Nothing
--   | otherwise = Just r -- (pX, pY)
--   where
--     denominator = fromIntegral ((x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4))
--     pX = fromIntegral
--           ((x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4))
--        / denominator
--     pY = fromIntegral
--           ((x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4))
--        / denominator

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

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

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
