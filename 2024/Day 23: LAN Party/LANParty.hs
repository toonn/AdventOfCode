module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Data.Char (isAlpha)
import Data.List (intercalate, maximumBy, sortOn)
import qualified Data.Map as M
import qualified Data.Set as S

newtype Password = Password (S.Set String)

type Input = [(String,String)]

node :: Parser String
node = takeWhile1P (Just "Letter") isAlpha

edge :: Parser (String,String)
edge = (,) <$> node <* char '-' <*> node

parser :: Parser Input
parser = sepEndBy1 edge eol <* eof

triangles :: Ord a => M.Map a (S.Set a) -> S.Set (S.Set a)
triangles graph = let potG = M.filter ((> 1) . length) graph
                      candidates = sortOn snd (M.assocs potG)
                   in foldr (\(a, es) more g ->
                              foldr (\(b, cs) ->
                                      ( S.map (`S.insert` (S.fromList [a,b]))
                                              (S.intersection es cs)
                                     <>
                                      )
                                    )
                                    mempty
                                    (M.assocs (M.restrictKeys g es))
                           <> more (M.delete a g)
                            )
                            (const mempty)
                            candidates
                            potG

graphFromEdges :: Input -> M.Map String (S.Set String)
graphFromEdges = foldr (\(a,b) -> M.insertWith (<>) a (S.singleton b)
                                . M.insertWith (<>) b (S.singleton a)
                       )
                       mempty

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = length
             . S.filter (('t' `elem`)
             . S.map (\(a:_) -> a))
             . triangles
             . graphFromEdges
           <$> input
  printAnswer "Triangles with a t: " answer

bronkerbosch :: Ord a => M.Map a (S.Set a) -> S.Set a -> S.Set a -> S.Set a
             -> S.Set (S.Set a)
bronkerbosch g r p x
  | null p, null x = S.singleton r
  | otherwise
  = foldr (\v more (r,p,x) ->
            let restrictN = S.intersection (g M.! v)
             in bronkerbosch g (S.insert v r) (restrictN p) (restrictN x)
             <> more (r, S.delete v p, S.insert v x)
          )
          (const mempty)
          p
          (r,p,x)

maximumClique :: Ord a => M.Map a (S.Set a) -> S.Set a
maximumClique g = maximumBy (curry (uncurry compare . both length))
                $ bronkerbosch g mempty (M.keysSet g) mempty

instance Show Password where
  show (Password p) = intercalate "," . S.toAscList $ p

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = Password . maximumClique . graphFromEdges <$> input
  printAnswer "Password to get into LAN: " answer

main :: IO ()
main = do
  let day = "Day 23: LAN Party"
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
