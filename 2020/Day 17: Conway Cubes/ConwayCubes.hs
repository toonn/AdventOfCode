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
type HyperCoord = (Integer, Integer, Integer, Integer)
type HyperCubes = S.Set HyperCoord

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space hspace1 empty empty)

integer :: Parser Integer
integer = lexeme L.decimal

row :: Parser (Integer -> S.Set (Integer, Integer))
row = do
  cells <- takeWhile1P (Just "Cube") (`elem` "#.")
  let coords y = foldr (\(x,c) cs -> if c == '#' then (x,y):cs else cs)
                       []
                       (zip [0..] cells)
  pure (S.fromList . coords)

cubes :: Parser (S.Set (Integer, Integer))
cubes = do
  rows <- sepEndBy row eol
  eof
  let initial = mconcat (zipWith ($) rows [0..])
  pure initial

readInput :: String -> IO (Parsed (S.Set (Integer, Integer)))
readInput day = do
  inputFile <- getDataFileName (day </> "input.txt")
  parse cubes inputFile
    <$> readFile inputFile

printAnswer :: Show a => String -> Parsed a -> IO ()
printAnswer question answer =
  either (putStrLn . errorBundlePretty)
         (putStrLn . (question <>) . show)
         answer

to3D :: S.Set (Integer, Integer) -> Cubes
to3D = S.map (\(x,y) -> (x,y,0))

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

simulate :: (cubes -> cubes) -> Integer -> cubes -> cubes
simulate _ 0 cubes = cubes
simulate step n cubes = simulate step (n - 1) (step cubes)

part1 :: Parsed (S.Set (Integer, Integer)) -> IO ()
part1 input = do
  let answer = S.size . simulate step 6 . to3D <$> input
  printAnswer "Active cubes after boot: " answer

to4D :: S.Set (Integer, Integer) -> HyperCubes
to4D = S.map (\(x,y) -> (x,y,0,0))

xyzws :: HyperCubes
      -> (S.Set Integer, S.Set Integer, S.Set Integer, S.Set Integer)
xyzws hypercubes =
  S.foldr (\(x,y,z,w) next (xs,ys,zs,ws) ->
            next (S.insert x xs, S.insert y ys, S.insert z zs, S.insert w ws)
          )
          id
          hypercubes
          (S.empty, S.empty, S.empty, S.empty)

bounds4 :: HyperCubes -> (HyperCoord, HyperCoord)
bounds4 hypercubes
  | S.null hypercubes = ((0,0,0,0),(0,0,0,0))
  | otherwise = ( (S.findMin xs, S.findMin ys, S.findMin zs, S.findMin ws)
                , (S.findMax xs, S.findMax ys, S.findMax zs, S.findMax ws)
                )
  where
    (xs, ys, zs, ws) = xyzws hypercubes

range4 :: (HyperCoord, HyperCoord) -> [HyperCoord]
range4 ((xm,ym,zm,wm),(xM,yM,zM,wM)) = do
  x <- [xm-1..xM+1]
  y <- [ym-1..yM+1]
  z <- [zm-1..zM+1]
  w <- [wm-1..wM+1]
  pure (x,y,z,w)

neighbors4 :: HyperCoord -> HyperCubes
neighbors4 hypercube =
  S.delete hypercube (S.fromList (range4 (hypercube,hypercube)))

collectLive4 :: HyperCubes -> HyperCoord
             -> (HyperCubes -> HyperCubes)
             -> HyperCubes -> HyperCubes
collectLive4 hypercubes hypercube next hcs
  | (active && (activeNeighbors == 2 || activeNeighbors == 3))
    || (not active && activeNeighbors == 3)
    = next (S.insert hypercube hcs)
  | otherwise = next hcs
  where
    active = S.member hypercube hypercubes
    activeNeighbors = S.size (neighbors4 hypercube `S.intersection` hypercubes)

step4 :: HyperCubes -> HyperCubes
step4 hypercubes = foldr (collectLive4 hypercubes)
                         id
                         (range4 (bounds4 hypercubes))
                         S.empty

part2 :: Parsed (S.Set (Integer, Integer)) -> IO ()
part2 input = do
  let answer = S.size . simulate step4 6 . to4D <$> input
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
