module Main where

import Criterion.Main
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import Data.Void (Void)
import System.FilePath ((</>))
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Paths_AoC2020

type Parser = Parsec Void String
type Parsed a = Either (ParseErrorBundle String Void) a

type Coord = (Integer, Integer, Integer)
type Cubes = S.Set Coord

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space hspace1 empty empty)

integer :: Parser Integer
integer = lexeme L.decimal

row :: Parser (Integer -> Cubes)
row = do
  cells <- takeWhile1P (Just "Cube") (`elem` "#.")
  let coords y = foldr (\(x,c) cs -> if c == '#' then (x,y,0):cs else cs)
                       []
                       (zip [0..] cells)
  pure (S.fromList . coords)

cubes :: Parser Cubes
cubes = do
  rows <- sepEndBy row eol
  eof
  let initial = mconcat (zipWith ($) rows [0..])
  pure initial

readInput :: String -> IO (Parsed Cubes)
readInput day = do
  inputFile <- getDataFileName (day </> "input.txt")
  parse cubes inputFile
    <$> readFile inputFile

printAnswer :: Show a => String -> Parsed a -> IO ()
printAnswer question answer =
  either (putStrLn . errorBundlePretty)
         (putStrLn . (question <>) . show)
         answer

xyzs :: Cubes -> (S.Set Integer, S.Set Integer, S.Set Integer)
xyzs cubes = S.foldr (\(x,y,z) next (xs,ys,zs) ->
                       next (S.insert x xs, S.insert y ys, S.insert z zs)
                     )
                     id
                     cubes
                     (S.empty, S.empty, S.empty)

bounds :: Cubes -> (Coord, Coord)
bounds cubes | S.null cubes = ((0,0,0),(0,0,0))
             | otherwise = ( (S.findMin xs, S.findMin ys, S.findMin zs)
                           , (S.findMax xs, S.findMax ys, S.findMax zs)
                           )
  where
    (xs, ys, zs) = xyzs cubes

range :: (Coord, Coord) -> [Coord]
range ((xm,ym,zm),(xM,yM,zM)) = do
  x <- [xm-1..xM+1]
  y <- [ym-1..yM+1]
  z <- [zm-1..zM+1]
  pure (x,y,z)

neighbors :: Coord -> Cubes
neighbors cube = S.delete cube (S.fromList (range (cube,cube)))

collectLive :: Cubes -> Coord -> (Cubes -> Cubes) -> Cubes -> Cubes
collectLive cubes cube next cs
  | (active && (activeNeighbors == 2 || activeNeighbors == 3))
    || (not active && activeNeighbors == 3)
    = next (S.insert cube cs)
  | otherwise = next cs
  where
    active = S.member cube cubes
    activeNeighbors = S.size (neighbors cube `S.intersection` cubes)

step :: Cubes -> Cubes
step cubes = foldr (collectLive cubes) id (range (bounds cubes)) S.empty

--simulate :: Integer -> Cubes -> Cubes
simulate 0 cubes = cubes
simulate n cubes = simulate (n - 1) (step cubes)

part1 :: Parsed Cubes -> IO ()
part1 input = do
  let answer = S.size . simulate 6 <$> input
  printAnswer "Active cubes after boot: " answer

part2 :: Parsed Cubes -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "Product of all six departure fields: " answer

main :: IO ()
main = do
  let day = "Day 17: Conway Cubes"
  input <- readInput day
  putStrLn ""
  part1 input
  part2 input
  putStrLn ""
  defaultMain [
      bgroup "AoC"
        [ bench "Part 1" $ nfIO (silence $ readInput day >>= part1)
        , bench "Part 2" $ nfIO (silence $ readInput day >>= part2)
        ]
    ]
