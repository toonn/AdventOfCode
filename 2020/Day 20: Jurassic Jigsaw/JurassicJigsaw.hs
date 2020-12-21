module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AoC

import Data.Maybe (fromJust)
import qualified Data.Monoid as M
import qualified Data.Vector as V

import Debug.Trace

data Tile = Tile Integer [[Char]]
  deriving Show
data Border = Border Integer [[Char]]
  deriving Show
type Image a = V.Vector (V.Vector a)
data Edge = T | R | B | L
  deriving Eq

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

cornerIDs :: Image Border -> [Integer]
cornerIDs image = [tl, tr, bl, br]
  where
    top = V.head image
    Border tl _ = V.head top
    Border tr _ = V.last top
    bottom = V.last image
    Border bl _ = V.head bottom
    Border br _ = V.last bottom

flipTile :: Border -> Border
flipTile (Border iD [top, right, bottom, left]) =
  Border iD [reverse top, reverse left, reverse bottom, reverse right]

turnLeft :: Int -> [a] -> [a]
turnLeft n es = drop n es <> take n es

rotate :: Border -> (Edge, [Char]) -> Maybe Border
rotate b@(Border iD edges@[top, right, bottom, left]) (side, edge)
  | top == edge' = case side of
    T -> Just (Border iD (turnLeft 2 edges))
    R -> Just (Border iD (turnLeft 1 edges))
    B -> Just b
    L -> Just (Border iD (turnLeft 3 edges))
  | right == edge' = case side of
    T -> Just (Border iD (turnLeft 3 edges))
    R -> Just (Border iD (turnLeft 2 edges))
    B -> Just (Border iD (turnLeft 1 edges))
    L -> Just b
  | bottom == edge' = case side of
    T -> Just b
    R -> Just (Border iD (turnLeft 3 edges))
    B -> Just (Border iD (turnLeft 2 edges))
    L -> Just (Border iD (turnLeft 1 edges))
  | left == edge' = case side of
    T -> Just (Border iD (turnLeft 3 edges))
    R -> Just b
    B -> Just (Border iD (turnLeft 1 edges))
    L -> Just (Border iD (turnLeft 2 edges))
  | otherwise      = Nothing
  where
    edge' = reverse edge

orient :: Border -> (Edge, [Char]) -> Maybe Border
orient b edge = case rotate b edge of
  Just b' -> Just b'
  Nothing -> case rotate (flipTile b) edge of
    Just b' -> Just b'
    Nothing -> Nothing

justs :: Int -> V.Vector (Maybe a) -> V.Vector (Int, Int, a)
justs y = V.ifoldr (\x ma rest -> case ma of
                     Just a -> V.cons (x,y,a) rest
                     Nothing -> rest
                   )
                   V.empty

firstJust :: V.Vector (Maybe a) -> (Int, a)
firstJust = V.ifoldr (\x ma next -> case ma of
                       Just a -> (x,a)
                       Nothing -> next
                     )
                     (error "No Just")

lastJust :: V.Vector (Maybe a) -> (Int, a)
lastJust = V.ifoldl (\next x ma -> case ma of
                      Just a -> (x,a)
                      Nothing -> next
                    )
                    (error "No Just")

orientSide :: Edge -> Border -> (Int, Int, Border) -> Maybe (Int, Int, Border)
           -> Maybe (Int, Int, Border)
orientSide side b (x,y,Border _ [to,ri,bo,le]) next =
  case orient b (side, edge) of
    Nothing -> next
    Just b' -> Just (x', y', b')
  where
    edge = case side of
             T -> to
             R -> ri
             B -> bo
             L -> le
    (x',y') = case side of
                T -> (x,y-1)
                R -> (x+1,y)
                B -> (x,y+1)
                L -> (x-1,y)

matchup :: Border -> Image (Maybe Border) -> Maybe (Image (Maybe Border))
matchup b partial = case topFit of
  Just (x,_,b') ->
    Just ( V.cons ( (V.replicate (V.length (V.head partial)) Nothing)
                    V.// [(x,Just b')]
                  )
                  partial
         )
  Nothing -> case bottomFit of
    Just (x,_,b') ->
      Just ( V.snoc partial
                    ( (V.replicate (V.length (V.head partial)) Nothing)
                      V.// [(x,Just b')]
                    )
           )
    Nothing -> case leftFit of
      Just (x,y,b')
        | x < 0 ->
          Just ( V.imap ( \y' row ->
                          V.cons (if y == y' then Just b' else Nothing) row
                        )
                        partial
               )
        | otherwise ->
          Just (partial V.// [(y, (partial V.! y) V.// [(x, Just b')])])
      Nothing -> case rightFit of
        Just (x,y,b')
          | x == V.length (V.head partial) ->
            Just ( V.imap
                     ( \y' row ->
                        V.snoc row (if y == y' then Just b' else Nothing)
                     )
                     partial
                 )
          | otherwise ->
            Just (partial V.// [(y, (partial V.! y) V.// [(x, Just b')])])
        Nothing -> Nothing
  where
    top = justs 0 (V.head partial)
    right = V.imap (\y row -> case lastJust row of (x,a) -> (x,y,a)) partial
    bottom = justs (V.length partial) (V.last partial)
    left = V.imap (\y row -> case firstJust row of (x,a) -> (x,y,a)) partial
    topFit = V.foldr (orientSide T b) Nothing top
    rightFit = V.foldr (orientSide R b) Nothing right
    bottomFit = V.foldr (orientSide B b) Nothing bottom
    leftFit = V.foldr (orientSide L b) Nothing left

finish :: [Border] -> [Border] -> Image (Maybe Border) -> Image (Maybe Border)
finish [] [] image = image
finish [] bs' partial = trace (show $ length bs') $ finish bs' [] partial
finish (b:bs) bs' partial = case matchup b partial of
  Nothing -> finish bs (b:bs') partial
  Just partial' -> finish bs bs' partial'

arrange :: [Border] -> Image Border
arrange [] = V.empty
arrange (b:bs) = V.map (V.map fromJust)
                       (finish bs [] (V.singleton (V.singleton (Just b))))

borders :: Tile -> Border
borders (Tile iD tile) = Border iD [top, right, bottom, left]
  where
    top = head tile
    right = map last tile
    bottom = reverse (last tile)
    left = reverse (map head tile)

part1 :: Parsed [Tile] -> IO ()
part1 input = do
  let answer = multiply . cornerIDs . arrange . map borders <$> input
  printAnswer "Product of corner tile IDs: " answer

part2 :: Parsed [Tile] -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "Not an answer: " answer

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
