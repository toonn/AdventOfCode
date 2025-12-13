module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import qualified Data.Map as M
import qualified Data.Set as S

type Input = (M.Map Int (S.Set YX), [((Int,Int), M.Map Int Int)])

shape :: Parser (Int, S.Set YX)
shape = (,) <$> integer <* char ':'
             <* eol
            <*> ( M.keysSet . M.filter (== '#') . foldYX
              <$> sepEndBy1 (takeWhile1P (Just "# or .") (`elem` "#.")) eol
                )

regionSpec :: Parser ((Int,Int), M.Map Int Int)
regionSpec = (,) <$> ((,) <$> integer <* char 'x' <*> integer)
                  <* lexeme (char ':')
                 <*> (M.fromAscList . zip [0..] <$> some integer)

parser :: Parser Input
parser = (,) <$> (M.fromAscList <$> sepEndBy1 (try shape) eol)
             <*> sepEndBy1 regionSpec eol
      <* eof

fitTrivial :: (YX, M.Map Int Int) -> Bool
fitTrivial ((y, x), shapesRequired) = sum shapesRequired
                                   <= (y `quot` 3) * (x `quot` 3)

-- Turns out the trivial answer is correct for part 1.
-- fitPotential :: M.Map Int (S.Set YX) -> (YX, M.Map Int Int) -> Bool
-- fitPotential shapes ((y, x), shapesRequired)
--   = sum (M.intersectionWith ((*) . length) shapes shapesRequired) <= y * x
--
-- -- Two shape 4's from my input would fit in a 4x3 region, that's the densest
-- -- possible packing so if assuming all the shapes are 4's still doesn't fit,
-- -- it's not possible.
-- --
-- -- What about packing some 4x3 and some 3x4?
-- fitDense :: (YX, M.Map Int Int) -> Bool
-- fitDense ((y, x), shapes) = uncurry (+) (sum shapes `quotRem` 2)
--                          <= max ((y `quot` 4) * (x `quot` 3))
--                                 ((y `quot` 3) * (x `quot` 4))

-- Too low:  440 (right answer for someone else)
-- Too high: 903
part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = length . filter fitTrivial . snd <$> input
  printAnswer "Regions that fit presents: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 12: Christmas Tree Farm"
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
