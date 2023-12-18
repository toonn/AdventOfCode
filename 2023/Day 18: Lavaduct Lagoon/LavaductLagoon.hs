module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Data.List (partition)
import qualified Data.Map as M
import qualified Data.Set as S

type Dir = Char
type Color = String
type Instruction = (Dir, Int, Color)
type Loop = (YX, S.Set YX, M.Map YX Color)

type Input = [Instruction]

instruction :: Parser Instruction
instruction = do
  dir <- lexeme anySingle
  distance <- integer
  color <- between (char '(')
                   (char ')')
                   (takeWhile1P (Just "# or hexit")
                                (`elem` "#0123456789abcdef")
                   )
  pure (dir, distance, color)

parser :: Parser Input
parser = sepEndBy1 instruction eol <* eof

digTrench :: YX -> Dir -> Int -> (YX, S.Set YX)
digTrench (y,x) dir distance =
  let ((yF,xF),(yT,xT)) | dir == 'U' = ((y - distance, x),(y - 1, x))
                        | dir == 'R' = ((y, x + 1),(y, x + distance))
                        | dir == 'D' = ((y + 1, x),(y + distance, x))
                        | dir == 'L' = ((y, x - distance),(y, x - 1))
      yx' | dir == 'U' = (yF,xF)
          | dir == 'R' = (yT,xT)
          | dir == 'D' = (yT,xT)
          | dir == 'L' = (yF,xF)
   in (yx', S.fromAscList [(yy,xx) | yy <- [yF..yT], xx <- [xF..xT]])

dig :: Loop -> Input -> Loop
dig loop [] = loop
dig (current, dug, colors) ((dir, distance, color):is) =
  let (next, trench) = digTrench current dir distance
      colors' = foldr (\yx more cs -> more (M.insert yx color cs))
                      id
                      trench
                      colors
   in dig (next, dug <> trench, colors') is

terrain :: S.Set YX -> (YX, YX, S.Set YX)
terrain loop =
  let yMin = fst (S.findMin loop)
      yMax = fst (S.findMax loop)
      xs = S.map snd loop
      xMin = S.findMin xs
      xMax = S.findMax xs
   in ( (yMin,xMin)
      , (yMax,xMax)
      , S.fromAscList [(y,x) | y <- [yMin..yMax], x <- [xMin..xMax]]
      )

neighbors :: YX -> S.Set YX
neighbors (y,x) = S.fromAscList [(y + dy, x + dx) | dy <- [-1..1]
                                                  , dx <- [-1..1]
                                                  , abs dy /= abs dx
                                ]

sortIntoBins :: (YX, YX, S.Set YX) -> [S.Set YX]
sortIntoBins ((yMin,xMin),(yMax,xMax),grid) =
  foldr (\yx more bins ->
          let nS = S.filter (\(y,x) ->
                              yMin <= y && y <= yMax && xMin <= x && x <= xMax
                            )
                            (neighbors yx)
              (otherBins, connectedBins) = partition (S.disjoint nS) bins
           in more (S.insert yx (mconcat connectedBins) : otherBins)
        )
        id
        grid
        []

digInterior :: S.Set YX -> S.Set YX
digInterior loop =
  let ((yMin,xMin),(yMax,xMax),grid) = terrain loop
      bins = sortIntoBins ((yMin,xMin), (yMax,xMax), grid S.\\ loop)
      border = S.fromAscList ( [(yMin,x) | x <- [xMin..xMax]]
                            <> [(y,x) | y <- [yMin + 1..yMax - 1]
                                      , x <- [xMin, xMax]
                               ]
                            <> [(yMax,x) | x <- [xMin..xMax]]
                             )
      interior = mconcat (filter (S.disjoint border) bins)
   in interior

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = length
             . (\(_,loop,_) -> loop <> digInterior loop)
             . dig ((0,0), S.singleton (0,0), mempty)
           <$> input
  printAnswer "Cubic meters of lava: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 18: Lavaduct Lagoon"
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
