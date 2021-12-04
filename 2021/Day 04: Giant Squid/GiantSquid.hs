module Main where

import Criterion.Main
import Data.List (delete)
import Data.Maybe (mapMaybe)
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.IntMap as M
import qualified Text.Megaparsec.Char.Lexer as L

import AoC

type Bingo = M.IntMap (Int, Int)
type Seen = M.IntMap Int

row :: Parser [Int]
row = hspace *> manyTill integer eol

board :: Parser Bingo
board = do
  numbers <- manyTill row (eol *> pure () <|> lookAhead eof)
  let board = foldr
        (\r next b y ->
          next (foldr (\n next' b' x -> next' (M.insert n (x,y) b') (x+1))
                      const
                      r
                      b
                      0
               )
               (y+1)
        )
        const
        numbers
        M.empty
        0
  pure board

bingoSetup :: Parser ([Int], [Bingo])
bingoSetup = do
  draws <- sepBy integer (char ',')
  eol
  eol
  boards <- manyTill board eof
  pure (draws, boards)

winningBoard :: ([Int], [Bingo]) -> (Int, Bingo)
winningBoard (draws, boards) =
  go 0 draws (map (\b -> (zeroes, zeroes, b)) boards)
  where
    zeroes = M.fromAscList (map (\a -> (a, 0)) [0..4])

    go :: Int -> [Int] -> [(Seen, Seen, Bingo)] -> (Int, Bingo)
    go last (d:ds) bs = case foldr (\(xs, ys, b) m ->
                                case M.foldr (\count m ->
                                               if count == 5
                                               then Just b
                                               else m
                                             )
                                             m
                                             xs of
                                  Nothing -> M.foldr (\count m ->
                                                       if count == 5
                                                       then Just b
                                                       else m
                                                     )
                                                     m
                                                     ys
                                  Just b -> Just b
                              )
                              Nothing
                              bs of
      Just b -> (last,b)
      Nothing -> go
        d
        ds
        (map (\(xs, ys, b) ->
               case M.updateLookupWithKey (\_ _ -> Nothing) d b of
                 (Nothing, b') -> (xs, ys, b')
                 (Just (x,y), b') ->
                   ( M.adjust (+ 1) x xs
                   , M.adjust (+ 1) y ys
                   , b'
                   )
             )
             bs
        )

score :: (Int, Bingo) -> Int
score (n, b) = n * (sum . M.keys $ b)

part1 :: Parsed ([Int], [Bingo]) -> IO ()
part1 input = do
  let answer = score . winningBoard <$> input
  printAnswer "Final score of the winning board: " answer

losingBoard :: ([Int], [Bingo]) -> (Int, Bingo)
losingBoard (draws, boards) =
  go 0 draws (map (\b -> (zeroes, zeroes, b)) boards)
  where
    zeroes = M.fromAscList (map (\a -> (a, 0)) [0..4])

    go :: Int -> [Int] -> [(Seen, Seen, Bingo)] -> (Int, Bingo)
    go last (d:ds) bs = case mapMaybe (\(xs, ys, b) ->
                                case M.foldr (\count m ->
                                               if count == 5
                                               then Nothing
                                               else m
                                             )
                                             (Just (xs,ys,b))
                                             xs of
                                  Just _ -> M.foldr (\count m ->
                                                       if count == 5
                                                       then Nothing
                                                       else m
                                                     )
                                                     (Just (xs,ys,b))
                                                     ys
                                  Nothing -> Nothing
                              )
                              bs of
      [] -> case bs of
              [(_,_,b)] -> (last, b)
      bs' -> go
        d
        ds
        (map (\(xs, ys, b) ->
               case M.updateLookupWithKey (\_ _ -> Nothing) d b of
                 (Nothing, b') -> (xs, ys, b')
                 (Just (x,y), b') ->
                   ( M.adjust (+ 1) x xs
                   , M.adjust (+ 1) y ys
                   , b'
                   )
             )
             bs'
        )

part2 :: Parsed ([Int], [Bingo]) -> IO ()
part2 input = do
  let answer = score . losingBoard <$> input
  printAnswer "Final score of the losing board: " answer

main :: IO ()
main = do
  let day = "Day 04: Giant Squid"
  let parser = bingoSetup
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
