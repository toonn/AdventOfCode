module Main where

import Data.Semigroup (Max(..))
import qualified Data.Set as S
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Paths_AoC2020

type Parser = Parsec Void String
type Parsed a = Either (ParseErrorBundle String Void) a

type Coord = Int
data TreeMap = TreeMap (Max Coord) (Max Coord) (S.Set (Coord, Coord))
  deriving Show

instance Semigroup TreeMap where
  (TreeMap x1 y1 t1) <> (TreeMap x2 y2 t2) =
    TreeMap (x1 <> x2) (y1 <> y2) (t1 <> t2)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space space1 empty empty)

treeOrEmpty :: Parser Bool
treeOrEmpty = (char '#' *> pure True) <|> (char '.' *> pure False)

row :: Parser (Max Coord, [Coord])
row = do
  trees <- manyTill treeOrEmpty eol
  pure $ foldr toCoord (\x -> (Max (x - 1), [])) trees 0
  where
    toCoord :: Bool -> (Coord -> (Max Coord, [Coord])) -> Coord
            -> (Max Coord, [Coord])
    toCoord tree rest x | tree = (x:) <$> xs
                        | otherwise = xs
      where xs = rest (x + 1)

parseLocalGeology :: Parser TreeMap
parseLocalGeology = do
  rows <- manyTill row eof
  pure $ foldr toTreeMap (\_ -> TreeMap 0 0 S.empty) rows 0
  where
    toTreeMap :: (Max Coord, [Coord]) -> (Coord -> TreeMap) -> Coord -> TreeMap
    toTreeMap (maxX, xs) rest y =
      TreeMap maxX (Max y) (S.fromList [ (x,y) | x <- xs ]) <> rest (y + 1)

readLocalGeology :: String -> IO (Parsed TreeMap)
readLocalGeology inputFile = parse parseLocalGeology inputFile
  <$> readFile inputFile

countTrees :: (Coord, Coord) -> TreeMap -> Integer
countTrees (stepX, stepY) (TreeMap (Max maxX) (Max maxY) trees) =
  foldr count
        0
        (zip [x `rem` (maxX + 1) | x <- [0,stepX..]] [0,stepY..maxY])
  where
    count :: (Coord, Coord) -> Integer -> Integer
    count loc n | loc `S.member` trees = n + 1
                | otherwise = n

main :: IO ()
main = do
  inputFile <- getDataFileName "Day 3: Toboggan Trajectory/input.txt"
  treeMap <- readLocalGeology inputFile
  let treeNr = countTrees (3,1) <$> treeMap
  either (putStrLn . errorBundlePretty)
         (putStrLn . ("Nr of trees along (3,1): " <>) . show)
         treeNr
