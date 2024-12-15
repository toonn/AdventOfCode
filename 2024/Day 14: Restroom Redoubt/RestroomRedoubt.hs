module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Control.Arrow ((***))
import Data.List (minimumBy, nub, sort)
import qualified Data.Set as S

type Robot = (YX,(Int,Int))

type Input = [Robot]

xyPair :: Parser (Int, Int)
xyPair = do x <- signed integer
            (char ',')
            y <- signed integer
            pure (y,x)

robotPV :: Parser Robot
robotPV = do string "p="
             p <- xyPair
             hspace
             string "v="
             v <- xyPair
             pure (p,v)

parser :: Parser Input
parser = sepEndBy1 robotPV eol <* eof

step :: Int -> Int -> Int -> Robot -> Robot
step ym xm times (yx,(vy,vx))
  = ( nTimes times (((`mod` ym) . (+ vy)) *** ((`mod` xm) . (+ vx))) yx
    , (vy,vx)
    )

quadrants :: Int -> Int -> [Robot] -> ([YX],[YX],[YX],[YX])
quadrants ym xm
  = let yb = ym `quot` 2
        xb = xm `quot` 2
     in foldr (\((y,x),_) ->
                \(nw,ne,sw,se) -> case (y `compare` yb, x `compare` xb) of
                 (LT,LT) -> ((y,x):nw,ne,sw,se)
                 (LT,GT) -> (nw,(y,x):ne,sw,se)
                 (GT,LT) -> (nw,ne,(y,x):sw,se)
                 (GT,GT) -> (nw,ne,sw,(y,x):se)
                 _ -> (nw,ne,sw,se)
              )
              ([],[],[],[])

safetyFactor :: ([YX],[YX],[YX],[YX]) -> Int
safetyFactor (nw,ne,sw,se) = length nw * length ne * length sw * length se

height :: Int
height = 103

width :: Int
width = 101

-- 104609262 too low (used example width and height on my input)
part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = safetyFactor
             . quadrants height width
             . map (step height width 100)
           <$> input
  printAnswer "Safety factor after 100 s: " answer

render :: Input -> String
render = {- ("\ESC[H" <>) -} id
       . (\ps -> foldr (\(py,px) more (cy,cx) ->
                         let l = py - cy
                             s | l == 0 = px - cx
                               | otherwise = px
                             res = replicate l '\n' <> replicate (s - 1) ' ' <> ['#']
                          in res <> more (py,px)
                       )
                       (const "") -- "\n------------------------------------------------------------------------\n\ESC[0J")
                       ps
                       (0,0)
         )
       . sort
       . nub
       . sort
       . map fst

neighbors :: YX -> S.Set YX
neighbors (y,x) = S.fromList [ (y + dy, x + dx) | dy <- [-1..1]
                                                , dx <- [-1..1]
                                                , dy /= 0 || dx /= 0
                             ]

vicinity :: S.Set YX -> YX -> Int
vicinity = (((9 -) . length) .) . (. neighbors) . S.intersection

overlap :: [Robot] -> Int
overlap robots | let rs = map fst robots, let rS = S.fromList rs
               = sum (map (vicinity rS) rs)

--             #
--            # #
--           #   #
--          #     #
--         ###   ###
--          #     #
--         #       #
--        #         #
--       #           #
--      ###         ###
--       #           #
--      #             #
--     #               #
--    #                 #
--   #                   #
part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = snd
             . minimumBy (\(r1,_) (r2,_) -> overlap r1 `compare` overlap r2)
             . flip zip [0..]
             . take (lcm height width)
             . iterate (map (step height width 1))
           <$> input
  -- (\(Right robots) -> mapM_ (putStrLn . render) robots) answer
  printAnswer "Fewest seconds until Easter egg: " answer

main :: IO ()
main = do
  let day = "Day 14: Restroom Redoubt"
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
