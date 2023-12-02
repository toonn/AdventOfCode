module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Char (isDigit, isSpace)
import qualified Data.Map as M
import qualified Data.Set as S

import AoC

type Coord = (Int, Int)
type Symbol = Char
data Element = Nr Int Int | Sym Symbol | Dots Int

type Input = (M.Map Coord (Int, Int), M.Map Coord Symbol)

nrDigits :: (Integral a, Show a) => a -> Int
nrDigits = length . show

element :: Parser Element
element = do (do setOffset 0
                 n <- integer
                 l <- getOffset
                 pure $ Nr n l)
             <|> (do setOffset 0
                     takeWhile1P (Just "dot") (== '.')
                     l <- getOffset
                     pure $ Dots l
                 )
             <|> (Sym <$> satisfy (\c -> not ( c == '.'
                                            || isDigit c
                                            || isSpace c)
                                  )
                 )

row :: Parser ([(Int, (Int, Int))], [(Int, Symbol)])
row = (\ts ->
        foldr (\t more (x, (nrs, symbols)) ->
                let r | Nr n l <- t = (x + l, (nrs <> [(x, (n,l))], symbols))
                      | Sym s  <- t = (x + 1, (nrs, symbols <> [(x, s)]))
                      | Dots l <- t = (x + l, (nrs, symbols))
                 in more r
              )
              snd
              ts
              (0, ([],[]))
      )
  <$> many element

toMap :: [[(Int, a)]] -> M.Map Coord a
toMap rows = foldr (\xValPairs more y ->
                     foldr (\(x,v) m -> M.insert (y,x) v m)
                           (more (y+1))
                           xValPairs
                   )
                   (const M.empty)
                   rows
                   0

parser :: Parser Input
parser = (\(nrs, symbols) -> (toMap nrs, toMap symbols))
       . unzip
     <$> sepEndBy row eol <* eof

border :: Coord -> (Int, Int) -> S.Set Coord
border (y, x) (n, l) = S.fromAscList ( map ((,) (y-1)) [x-1..x+l]
                                    <> [(y,x-1),(y,x+l)]
                                    <> map ((,) (y+1)) [x-1..x+l]
                                     )

symbolAdjacent :: M.Map Coord Symbol -> S.Set Coord -> Bool
symbolAdjacent symbols = not . S.null . S.intersection (M.keysSet symbols)

partNumbers :: Input -> [Int]
partNumbers (nrs, symbols) = M.elems
                           . M.mapMaybe (\(n, b) ->
                                          let n' | symbolAdjacent symbols b
                                                 = Just n
                                                 | otherwise
                                                 = Nothing
                                           in n'
                                        )
                           $ M.mapWithKey (\c (n,l) -> (n, border c (n,l))) nrs

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . partNumbers <$> input
  printAnswer "Sum of part numbers: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const "P" <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 03: Gear Ratios"
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
