module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec hiding (chunk)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AoC

type TargetArea = (Int, Int, Int, Int)

parser :: Parser TargetArea
parser = do
  lexeme (string "target area:")
  string "x="
  xm <- signed integer
  string ".."
  xM <- signed integer
  lexeme (char ',')
  string "y="
  ym <- signed integer
  string ".."
  yM <- signed integer
  eol
  eof
  pure (xm, xM, ym, yM)

triangular :: Int -> Int
triangular n | n < 0 = - triangular (abs n)
             | otherwise = (n * (n + 1)) `quot` 2

velocityRangeX :: TargetArea -> [Int]
velocityRangeX (xm, xM, _, _) = ns
  where
    minV | xm < 0 = xm
         | otherwise = 1
    maxV | xM > 0 = xM
         | otherwise = -1
    ns = foldr (\n ns -> if any (\t -> xm <= t && t <= xM)
                                (map (\n' -> triangular n - triangular n')
                                     [0,signum n..n]
                                )
                         then n:ns
                         else ns
               )
               []
               [minV..maxV]

velocityRangeY :: TargetArea -> [Int]
velocityRangeY (_, _, ym, yM) = ns
  where
    minV | ym < 0 = ym
         | otherwise = foldr (\t next v -> if t >= ym then v else next (v+1))
                             (error ( "There is no number greater than the"
                                   <> " greatest triangular number."
                                    )
                             )
                             (map triangular [1..])
                             1
    maxV | yM < 0 = abs ym - 1
         | ym > 0 = yM
         | otherwise = error "Should deal with this : /"
    ns = [maxV, maxV - 1..minV]

locations :: Int -> Int -> [(Int, Int)]
locations vX vY = scanl1 (\(a,b) (c,d) -> (a+c,b+d))
                         (iterate (\(vX, vY) ->
                                    (signum vX * (abs vX - 1), vY - 1)
                                  )
                                  (vX, vY)
                         )

crossesTarget :: TargetArea -> (Int, Int) -> Bool
crossesTarget (xm, xM, ym, yM) (vX, vY) =
  foldr (\((x,y), (x',y')) next -> case y > y' of
          True | y < ym -> False
          _ | xm <= x && x <= xM && ym <= y && y <= yM -> True
            | otherwise -> next
        )
        (error  ( "At some point the projectile has to either enter the target"
               <> " area or go below it while going down."
                )
        )
        (zip ls (tail ls))
  where
    ls = locations vX vY

part1 :: Parsed TargetArea -> IO ()
part1 input = do
  let answer = (\vy -> if vy < 0 then 0 else triangular vy)
             . snd
             . head
             . (\target ->
                 filter (crossesTarget target)
                        (concatMap (\vy -> map (\vx -> (vx,vy))
                                               (velocityRangeX target)
                                   )
                                   (velocityRangeY target)
                        )
               )
           <$> input
  printAnswer "Zenith of highest trajectory: " answer

part2 :: Parsed TargetArea -> IO ()
part2 input = do
  let answer = length
             . (\target ->
                 filter (crossesTarget target)
                        (concatMap (\vy -> map (\vx -> (vx,vy))
                                               (velocityRangeX target)
                                   )
                                   (velocityRangeY target)
                        )
               )
           <$> input
  printAnswer "Distinct initial velocities: " answer

main :: IO ()
main = do
  let day = "Day 17: Trick Shot"
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
