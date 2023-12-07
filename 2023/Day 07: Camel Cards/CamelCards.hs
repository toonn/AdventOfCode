module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.List (sortBy)
import qualified Data.Map as M

import AoC

type Card = Int
type Hand = [Card]
type HandType = Int

type Input = [(Hand, Int)]

faceValues :: M.Map Char Int
faceValues = M.fromList (zip "AKQJT98765432" [14,13..2])

hand :: Parser Hand
hand = map (faceValues M.!)
   <$> takeWhile1P (Just "card") (`elem` (M.keys faceValues))

handBid :: Parser (Hand, Int)
handBid = (,) <$> lexeme hand <*> integer

parser :: Parser Input
parser = sepEndBy handBid eol <* eof

handType :: Hand -> HandType
handType = (\counts ->
             let highest = maximum (M.elems counts)
                 hType | highest == 5 = 7
                       | highest == 4 = 6
                       | highest == 3, M.size counts == 2 = 5
                       | highest == 3, M.size counts == 3 = 4
                       | highest == 2, M.size counts == 3 = 3
                       | otherwise = highest
              in hType
           )
         . foldr (\c -> M.insertWith (+) c 1)
                 M.empty

compareHands :: M.Map Hand HandType -> Hand -> Hand -> Ordering
compareHands types h1 h2 = let t1 = types M.! h1
                               t2 = types M.! h2
                               ordering | t1 < t2 = LT
                                        | t1 > t2 = GT
                                        | otherwise = h1 `compare` h2
                            in ordering

totalWinnings :: (Hand -> HandType) -> Input -> Int
totalWinnings hT hs = let handBids = M.fromList hs
                          types = M.mapWithKey (const . hT) handBids
                          hands = M.keys handBids
                          ranked = sortBy (compareHands types) hands
                       in foldr (\(r, h) tot -> r * (handBids M.! h) + tot)
                                0
                                (zip [1..] ranked)

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = totalWinnings handType <$> input
  printAnswer "Total winnings: " answer

jokerFixup :: Input -> Input
jokerFixup = map (\(h,b) -> (map (\v -> if v == 11 then 1 else v) h, b))

jokerType :: Hand -> HandType
jokerType = (\counts ->
              let jokerCount = M.findWithDefault 0 1 counts
                  jokerless = M.delete 1 counts
                  highest = jokerCount + maximum (0 : M.elems jokerless)
                  hType | highest == 5 = 7
                        | highest == 4 = 6
                        | highest == 3, M.size jokerless == 2 = 5
                        | highest == 3, M.size jokerless == 3 = 4
                        | highest == 2, M.size jokerless == 3 = 3
                        | otherwise = highest
               in hType
            )
          . foldr (\c -> M.insertWith (+) c 1)
                  M.empty

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = totalWinnings jokerType . jokerFixup <$> input
  printAnswer "Total winnings with joker: " answer

main :: IO ()
main = do
  let day = "Day 07: Camel Cards"
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
