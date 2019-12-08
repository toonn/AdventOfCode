#! /usr/bin/env nix-shell
#! nix-shell -i runghc
#! nix-shell -p "haskellPackages.ghcWithPackages (ps: with ps; [ containers ])"

module DigitalSendingNetwork where

import Text.ParserCombinators.ReadP
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Set as S

type Pixel = Int
type Row = [Pixel]
type Layer = [Row]
type Layers = [Layer]

nr :: Layer -> Pixel -> Int
nr l p = sum . map (length . filter (== p)) $ l

fewest0 :: Layers -> Layer
fewest0 = snd . M.findMin
  . foldr (\l m -> M.insert (nr l 0) l m)
          M.empty

checksum :: Layers -> Int
checksum = (\l -> nr l 1 * nr l 2) . fewest0

flatten :: Layers -> Layer
flatten = foldr (zipWith (zipWith leftBiased)) (repeat [2..])
  where
    leftBiased a b | a == 2 = b
                   | otherwise = a

prettyPrint :: Layer -> String
prettyPrint = intercalate "\n" . map (map ("░█ " !!))

prefix :: String -> String -> String
prefix p = (p <>) . concatMap indent
  where
    indent '\n' = '\n':replicate (length p) ' '
    indent c = [c]

parseRow :: Int -> ReadP Row
parseRow columns = mapM (\_ -> read . (:[]) <$> get) [1..columns]

parseLayer :: Int -> Int -> ReadP Layer
parseLayer columns rows = mapM (\_ -> parseRow columns) [1..rows]

parseLayeredImage :: Int -> Int -> String -> Layers
parseLayeredImage columns rows =
  fst . last . readP_to_S (many (parseLayer columns rows))

main :: IO ()
main = do
  layers <- parseLayeredImage 25 6 <$> readFile "input.txt"
  putStrLn . ("Checksum (fewest 0's, 1's * 2's): " <>) . show $
    checksum layers
  putStrLn .  prefix "Decoded image: " . prettyPrint . flatten $ layers
