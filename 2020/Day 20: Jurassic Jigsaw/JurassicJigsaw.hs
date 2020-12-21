module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AoC

import Data.List (transpose)
import qualified Data.Map as M
import qualified Data.Monoid as M
import qualified Data.Set as S

data Tile = Tile Integer [[Char]]
  deriving Show
data Border = Border Integer [[Char]]
  deriving Show
type Image = S.Set (Integer, Integer)
data Direction = Three | Six | Nine | Twelve | Three' | Six' | Nine' | Twelve'

tile :: Parser Tile
tile = do
  lexeme (string "Tile")
  iD <- integer
  lexeme (char ':')
  eol
  pixels <- sepEndBy (takeWhile1P (Just "Pixel") (`elem` "#.")) eol
  pure $ Tile iD pixels

tiles :: Parser [Tile]
tiles = sepEndBy tile eol <* eof

multiply :: Num a => [a] -> a
multiply = M.getProduct . mconcat . map M.Product

cornerIDs :: M.Map [Char] [Integer] -> [Integer]
cornerIDs m = M.foldr ( \(iD:rest) next seen ->
                          if null rest
                          then if S.member iD seen
                               then iD:next seen
                               else next (S.insert iD seen)
                          else next seen
                      )
                      (const [])
                      m
                      S.empty

canonicalize :: Ord a => [a] -> [a]
canonicalize as = let as' = reverse as in case compare as as' of
  LT -> as
  EQ -> as
  GT -> as'

tally :: M.Map [Char] [Integer] -> [Border] -> M.Map [Char] [Integer]
tally m [] = m
tally m (Border iD edges:bs) =
  tally ( foldr (\e m' -> M.insertWith (\[x] xs -> x:xs) e [iD] m')
                m
                (map canonicalize edges)
        )
        bs

borders :: Tile -> Border
borders (Tile iD tile) = Border iD [top, right, bottom, left]
  where
    top = head tile
    right = map last tile
    bottom = reverse (last tile)
    left = reverse (map head tile)

part1 :: Parsed [Tile] -> IO ()
part1 input = do
  let answer = multiply . cornerIDs . tally M.empty . map borders <$> input
  printAnswer "Product of corner tile IDs: " answer

orientCorner :: M.Map [Char] [Integer] -> Tile -> Tile
orientCorner edgeMap (Tile iD tile) =
  case ( length (edgeMap M.! canonicalize (head tile))
       , length (edgeMap M.! canonicalize (map head tile))
       ) of
    (1, 1) -> Tile iD tile
    (2, 1) -> Tile iD (reverse tile)
    (1, 2) -> Tile iD (map reverse tile)
    (2, 2) -> Tile iD (reverse (map reverse tile))

rotateRight :: [[a]] -> [[a]]
rotateRight = map reverse . transpose

rotateLeft :: [[a]] -> [[a]]
rotateLeft = reverse . transpose

orientTopEdge :: M.Map [Char] [Integer] -> [Char] -> Tile -> Tile
orientTopEdge edgeMap edge (Tile iD tile) =
  case ( length (edgeMap M.! canonicalize top)
       , length (edgeMap M.! canonicalize bottom)
       , length (edgeMap M.! canonicalize left)
       , length (edgeMap M.! canonicalize right)
       ) of
    (1,_,_,_) | left == edge         -> Tile iD tile
              | otherwise            -> Tile iD (map reverse tile)
    (_,1,_,_) | reverse left == edge -> Tile iD (reverse tile)
              | otherwise            -> Tile iD (map reverse (reverse tile))
    (_,_,1,_) | bottom == edge       -> Tile iD (rotateRight tile)
              | otherwise            -> Tile iD (rotateRight (reverse tile))
    (_,_,_,1) | reverse top == edge  -> Tile iD (rotateLeft tile)
              | otherwise            -> Tile iD (rotateLeft (reverse tile))
  where
    top = head tile
    bottom = last tile
    left = map head tile
    right = map last tile

topRow :: M.Map [Char] [Integer] -> M.Map Integer Tile -> Tile
       -> [Tile]
topRow edgeMap tileMap t@(Tile iD tile) = t : case tiles of
  [iD] -> []
  iDs -> let iD' = head (filter (/= iD) iDs)
             tile' = tileMap M.! iD'
          in topRow edgeMap
                    tileMap
                    (orientTopEdge edgeMap edge tile')
  where
    edge = map last tile
    tiles = edgeMap M.! canonicalize edge

orient :: [Char] -> Tile -> Tile
orient edge (Tile iD tile)
  | edge == top = Tile iD tile
  | edge == reverse top = Tile iD (map reverse tile)
  | edge == bottom = Tile iD (reverse tile)
  | edge == reverse bottom = Tile iD (map reverse (reverse tile))
  | edge == left = Tile iD (rotateRight (reverse tile))
  | edge == reverse left = Tile iD (rotateRight tile)
  | edge == right = Tile iD (rotateLeft tile)
  | edge == reverse right = Tile iD (rotateLeft (reverse tile))
  where
    top = head tile
    bottom = last tile
    left = map head tile
    right = map last tile

fitBelow :: M.Map [Char] [Integer] -> M.Map Integer Tile
         -> Tile -> [Tile] -> [Tile]
fitBelow edgeMap tileMap (Tile iD tile) rest = case tiles of
  [] -> []
  [iD'] -> let tile' = tileMap M.! iD'
            in orient edge tile' : rest
  where
    edge = last tile
    tiles = filter (/= iD) (edgeMap M.! canonicalize edge)

extend :: M.Map [Char] [Integer] -> M.Map Integer Tile -> [Tile] -> [[Tile]]
extend _ _ [] = []
extend edgeMap tileMap row = row : extend edgeMap tileMap row'
  where
    row' = foldr (fitBelow edgeMap tileMap) [] row

crop :: [[Char]] -> [[Char]]
crop = map (init . tail) . init . tail

coordinates :: [[[[Char]]]] -> Image
coordinates  =
  foldr (\(r,row) rest ->
           foldr (\(c,col) rest' ->
                    foldr (\(y,row') rest'' ->
                             foldr (\(x,pixel) rest''' ->
                                      let l = fromIntegral (length row')
                                       in if pixel == '#'
                                          then S.insert (x + l*c, y + l*r)
                                                        rest'''
                                          else rest'''
                                   )
                                   rest''
                                   (zip [0..] row')
                          )
                          rest'
                          (zip [0..] col)
                 )
                 rest
                 (zip [0..] row)
        )
        S.empty
            . zip [0..]

arrange :: [Tile] -> (Integer, Image)
arrange tiles = (dimension, image)
  where
    edgeMap = tally M.empty (map borders tiles)
    startTile = head (cornerIDs edgeMap)
    tileMap = M.fromList (map (\t@(Tile iD _) -> (iD, t)) tiles)
    top = topRow edgeMap
                 tileMap
                 (orientCorner edgeMap (tileMap M.! startTile))
    composed = extend edgeMap tileMap top
    cropped = map (map (\(Tile _ tile) -> crop tile)) composed
    image = coordinates cropped
    dimension = fromIntegral (length cropped * length (head (head cropped)))

seaMonster :: Direction -> (Integer, Integer) -> S.Set (Integer,Integer)
seaMonster Three (x,y) = S.fromList (head:(belly <> back))
  where
    head = (x+18,y-1)
    belly = [(x+x',y+1) | x' <- [1,4..16]]
    back =  [(x+x',y) | x' <- [0,5,6,11,12,17,18,19]]
seaMonster Six (x,y) = S.fromList (head:(belly <> back))
  where
    head = (x+1,y+18)
    belly = [(x-1,y+x') | x' <- [1,4..16]]
    back =  [(x,y+x') | x' <- [0,5,6,11,12,17,18,19]]
seaMonster Nine (x,y) = S.fromList (head:(belly <> back))
  where
    head = (x-18,y+1)
    belly = [(x-x',y-1) | x' <- [1,4..16]]
    back =  [(x-x',y) | x' <- [0,5,6,11,12,17,18,19]]
seaMonster Twelve (x,y) = S.fromList (head:(belly <> back))
  where
    head = (x-1,y-18)
    belly = [(x+1,y-x') | x' <- [1,4..16]]
    back =  [(x,y-x') | x' <- [0,5,6,11,12,17,18,19]]
seaMonster Three' (x,y) = S.fromList (head:(belly <> back))
  where
    head = (x+18,y+1)
    belly = [(x+x',y-1) | x' <- [1,4..16]]
    back =  [(x+x',y) | x' <- [0,5,6,11,12,17,18,19]]
seaMonster Six' (x,y) = S.fromList (head:(belly <> back))
  where
    head = (x-1,y+18)
    belly = [(x+1,y+x') | x' <- [1,4..16]]
    back =  [(x,y+x') | x' <- [0,5,6,11,12,17,18,19]]
seaMonster Nine' (x,y) = S.fromList (head:(belly <> back))
  where
    head = (x-18,y-1)
    belly = [(x-x',y+1) | x' <- [1,4..16]]
    back =  [(x-x',y) | x' <- [0,5,6,11,12,17,18,19]]
seaMonster Twelve' (x,y) = S.fromList (head:(belly <> back))
  where
    head = (x+1,y-18)
    belly = [(x-1,y-x') | x' <- [1,4..16]]
    back =  [(x,y-x') | x' <- [0,5,6,11,12,17,18,19]]

seaMonsters :: Integer -> (Integer, Integer) -> [S.Set (Integer, Integer)]
seaMonsters d (x,y) =
  three <> six <> nine <> twelve <> three' <> six' <> nine' <> twelve'
  where
    three | x + 19 < d && y - 1 >= 0 && y + 1 < d = [seaMonster Three (x,y)]
          | otherwise = []
    three' | x + 19 < d && y - 1 >= 0 && y + 1 < d = [seaMonster Three' (x,y)]
           | otherwise = []
    six | y + 19 < d && x - 1 >= 0 && x + 1 < d = [seaMonster Six (x,y)]
        | otherwise = []
    six' | y + 19 < d && x - 1 >= 0 && x + 1 < d = [seaMonster Six' (x,y)]
         | otherwise = []
    nine | x - 19 >= 0 && y - 1 >= 0 && y + 1 < d = [seaMonster Nine (x,y)]
         | otherwise = []
    nine' | x - 19 >= 0 && y - 1 >= 0 && y + 1 < d = [seaMonster Nine' (x,y)]
          | otherwise = []
    twelve | y - 19 >= 0 && x - 1 >= 0 && x + 1 < d = [seaMonster Twelve (x,y)]
           | otherwise = []
    twelve'
      | y - 19 >= 0 && x - 1 >= 0 && x + 1 < d = [seaMonster Twelve' (x,y)]
      | otherwise = []

findMonsters :: Integer -> Image -> Image
findMonsters dimension image =
  S.foldr (\pixel monsters ->
            (foldr (\monster rest ->
                     if monster `S.isSubsetOf` image
                     then monster
                     else rest
                   )
                   S.empty
                   (seaMonsters dimension pixel)
            ) <> monsters
          )
          S.empty
          image

part2 :: Parsed [Tile] -> IO ()
part2 input = do
  let answer = S.size
             . (\(dim,image) -> image S.\\ findMonsters dim image)
             . arrange
           <$> input
  printAnswer "Water roughness: " answer

main :: IO ()
main = do
  let day = "Day 20: Jurassic Jigsaw"
  let parser = tiles
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
