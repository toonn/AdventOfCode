module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.IntMap as IM
import Data.List (intercalate)
import Data.Maybe (fromJust)

import AoC

data Instruction = NoOp | AddX Int deriving Show
type RegisterHistory = IM.IntMap Int
type Cycle = Int
newtype CRT = CRT { unCRT :: String }

instance Show CRT where
  show = unCRT

type Input = [Instruction]

instruction :: Parser Instruction
instruction = (string "noop" *> pure NoOp)
          <|> (AddX <$> (lexeme (string "addx") *> signed integer))

parser :: Parser Input
parser = manyTill (instruction <* eol) eof

value :: Cycle -> RegisterHistory -> Int
value c = snd . fromJust . IM.lookupLE c

execute :: Instruction -> (Cycle, RegisterHistory) -> (Cycle, RegisterHistory)
execute NoOp     (c, rH) = (c + 1, rH)
execute (AddX v) (c, rH)
  | let c' = c + 2 = (c', IM.insert (c' + 1) (value c' rH + v) rH)

interpret :: [Instruction] -> RegisterHistory
interpret instructions = foldr (\i more history ->
                                 more (execute i history)
                               )
                               snd
                               instructions
                               (0, IM.singleton 1 1)

signalStrengths :: RegisterHistory -> [Int]
signalStrengths registerHistory
  = [c * (value c registerHistory) | c <- [20,60,100,140,180,220]]

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . signalStrengths . interpret <$> input
  printAnswer "Sum of the six signal strengths: " answer

renderCRT :: RegisterHistory -> CRT
renderCRT registerHistory
  = CRT ( intercalate "\n"
                      ( map (\(row, pixels) ->
                              map (\x ->
                                    let c = row * 40 + x + 1
                                        spriteX = value c registerHistory
                                        pixel | abs (spriteX - x) <= 1 = '#'
                                              | otherwise = '.'
                                     in pixel
                                  )
                                  pixels
                            )
                            (zip [0..] (replicate 6 [0..39]))
                      )
        )

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = renderCRT . interpret <$> input
  printAnswer "Eight capital letters on CRT:\n" answer

main :: IO ()
main = do
  let day = "Day 10: Cathode-Ray Tube"
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
