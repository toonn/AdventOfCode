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
                          | '@' <- t' = error "Ran into myself"
                          | otherwise = error "Out of bounds"
                   in res

execute :: (Char -> YX -> M.Map YX Char -> (YX, M.Map YX Char))
        -> [Char]
        -> YX
        -> M.Map YX Char
        -> M.Map YX Char
execute _ [] _ grid = grid
execute transform (m:ms) yx grid
  = uncurry (execute transform ms) ((<> grid) <$> transform m yx grid)

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
               . M.filter (`elem` "O[")

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
                  in execute shove instructions start grid
               )
           <$> input
  -- putStrLn . render . (\(Right r) -> r) $ answer
  printAnswer "Sum of GPS coordinates: " answer

scaleUp :: [[Char]] -> [[Char]]
scaleUp = map (foldMap (\c -> case c of
                                '#' -> "##"
                                'O' -> "[]"
                                '.' -> ".."
                                '@' -> "@."
                       )
              )

shovees :: Char -> M.Map YX Char -> M.Map YX Char -> Maybe (M.Map YX Char)
shovees m yxs grid
  | null yxs = pure mempty
  | otherwise
  = let moved = M.mapKeys (move m) yxs M.\\ yxs
        addHalves | m `elem` "^v"
                  = M.foldMapWithKey
                      (\yx t ->
                        let insHalf | t == '[' = M.insert ((+ 1) <$> yx) ']'
                                    | t == ']'
                                    = M.insert ((subtract 1) <$> yx) '['
                                    | otherwise = id
                         in insHalf (M.singleton yx t)
                      )
                  | otherwise = id
        yxs' = addHalves (grid `M.intersection` moved)
        res | not (null (moved M.\\ grid)) = error "Out of bounds"
            | any (== '@') yxs' = error "I must've gone loopy"
            | any (== '#') yxs' = Nothing
            | otherwise = (yxs <>) <$> shovees m (M.filter (/= '.') yxs') grid
     in res

shove2 :: Char -> YX -> M.Map YX Char -> (YX, M.Map YX Char)
shove2 m yx grid | Just shoved <- shovees m (M.singleton yx '@') grid
                 = let dots = (const '.') <$> shoved
                       moved = M.mapKeys (move m) shoved
                    in (move m yx, moved <> dots <> grid)
                 | otherwise = (yx, grid)

-- 1568579 -- too high
-- 1553294 -- too low
part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = sum
             . gpsCoordinates
             . (\(warehouse, instructions) ->
                 let grid = foldYX (scaleUp warehouse)
                     start = foldr (\(yx,t) next ->
                                     if t == '@' then yx else next
                                   )
                                   (error "No robot")
                                   (M.assocs grid)
                  in execute shove2 instructions start grid
               )
           <$> input
  -- putStrLn . render . (\(Right r) -> r) $ answer
  printAnswer "Scaled-up GPS sum: " answer

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
