module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import qualified Data.Map as M

type Input = ([[Char]],[Char])

movements :: Parser [Char]
movements = some ((oneOf "^>v<") <* space)

parser :: Parser Input
parser = (,) <$> characterGrid <* eol <*> movements <* eof

move :: Char -> YX -> YX
move '^' (y,x) = (y - 1, x)
move '>' (y,x) = (y, x + 1)
move 'v' (y,x) = (y + 1, x)
move '<' (y,x) = (y, x - 1)

shove :: Char -> YX -> M.Map YX Char -> (YX, M.Map YX Char)
shove m yx grid | Just t <- grid M.!? yx
                , let yx' = move m yx
                , Just t' <- grid M.!? yx'
                = let (yx'', changes) = shove m yx' grid
                      change cs = (yx', M.insert yx '.' . M.insert yx' t $ cs)
                      res | t' == '#' || t' == 'O' && yx' == yx'' = (yx, mempty)
                          | '.' <- t' = change mempty
                          | 'O' <- t' = change changes
                          | '@' <- t' = error "Out of bounds"
                   in res

execute :: [Char] -> YX -> M.Map YX Char -> M.Map YX Char
execute [] _ grid = grid
execute (m:ms) yx grid = uncurry (execute ms) ((<> grid) <$> shove m yx grid)

render :: M.Map YX Char -> String
render grid = foldr (\((y,_),t) more lastY ->
                      (if y > lastY then ('\n' :) else id) (t : more y)
                    )
                    (const "\n")
                    (M.assocs grid)
                    0
gpsCoordinates :: M.Map YX Char -> [Int]
gpsCoordinates = map (\(y,x) -> 100 * y + x)
               . M.keys
               . M.filter (== 'O')

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum
             . gpsCoordinates
             . (\(warehouse, instructions) ->
                 let grid = foldYX warehouse
                     start = foldr (\(yx,t) next ->
                                     if t == '@' then yx else next
                                   )
                                   (error "No robot")
                                   (M.assocs grid)
                  in execute instructions start grid
               )
           <$> input
  -- putStrLn . render . (\(Right r) -> r) $ answer
  printAnswer "Sum of GPS coordinates: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 15: Warehouse Woes"
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
