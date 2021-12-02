module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AoC

data Instruction = Forward Int | Down Int | Up Int

type Aimed = (Int, (Int, Int))
type Position = (Int, Int)

instruction :: Parser Instruction
instruction = do
  direction <- lexeme (many letterChar)
  let directionC = case direction of
                     "forward" -> Forward
                     "down" -> Down
                     "up" -> Up
                     s -> error s
  distance <- integer
  eol
  pure (directionC distance)

instructions :: Parser [Instruction]
instructions = manyTill instruction eof

move :: Position -> [Instruction] -> Position
move start is = foldr (\i next (x,y) ->
                        next (case i of
                               Forward d -> (x + d, y    )
                               Down    d -> (x    , y + d)
                               Up      d -> (x    , y - d)
                             )
                      ) id is start

part1 :: Parsed [Instruction] -> IO ()
part1 input = do
  let answer = (\(x,y) -> x * y) . move (0,0) <$> input
  printAnswer "Coordinate product: " answer

aimedMove :: [Instruction] -> Aimed
aimedMove is = foldr (\i next (aim, (x,y)) ->
                        next (case i of
                               Forward d -> (aim    , (x + d, y + d * aim))
                               Down    d -> (aim + d, (x    , y          ))
                               Up      d -> (aim - d, (x    , y          ))
                             )
                      ) id is (0, (0,0))

part2 :: Parsed [Instruction] -> IO ()
part2 input = do
  let answer = (\(x,y) -> x * y) . snd . aimedMove <$> input
  printAnswer "Aimed coordinate product: " answer

main :: IO ()
main = do
  let day = "Day 02: Dive!"
  let parser = instructions
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
