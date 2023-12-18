module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Data.Char (digitToInt)
import Data.List (elemIndex, partition)
import qualified Data.Map as M
import qualified Data.Set as S

type Dir = Char
type Color = String
type Instruction = (Dir, Int, Color)
type Loop = (YX, S.Set YX)

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

dropColor :: Instruction -> (Dir, Int)
dropColor (dir, distance, _) = (dir, distance)

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = area . map dropColor <$> input
  printAnswer "Cubic meters of lava: " answer

extractInstruction :: Instruction -> (Dir, Int)
extractInstruction (_,_,'#':color) =
  ("RDLU" !! digitToInt (last color), read ("0x" <> init color))

vertices :: [(Dir, Int)] -> [YX]
vertices instructions = foldr (\(dir, distance) more (y,x) ->
                                let (y',x') | dir == 'R' = (y, x + distance)
                                            | dir == 'D' = (y + distance, x)
                                            | dir == 'L' = (y, x - distance)
                                            | dir == 'U' = (y - distance, x)
                                 in (y,x) : more (y',x')
                              )
                              (const [])
                              instructions
                              (0,0)

-- | Calculate the area of a simple, i.e. non self-intersecting, polygon
--
-- Based on a [formula from Wikipedia](
-- https://en.wikipedia.org/wiki/Polygon#Simple_polygons)
-- similar to the Shoelace formula.
area :: [(Dir, Int)] -> Int
area instructions =
  let polygon = vertices instructions
      trench = 1 + (sum (map snd instructions)) `quot` 2
   in trench
    + foldr (\(yI1,xI1) more a (yI,xI) ->
              more (a + xI * yI1 - xI1 * yI) (yI1, xI1)
            )
            (const . (\n -> let (q,0) = n `quotRem` 2 in q) . abs)
            (tail polygon <> [head polygon])
            0
            (head polygon)

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = area . map extractInstruction <$> input
  printAnswer "Cubic meters of lava with the correct instructions: " answer

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
