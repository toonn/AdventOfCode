module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.List (elemIndices, intercalate)
import qualified Data.Set as S

import AoC

type Coord = (Int, Int)
type Rock = S.Set Coord
type JetPattern = String

newtype RockString = RS { unRS :: String }

instance Show RockString where
  show = unRS

type Input = JetPattern

parser :: Parser Input
parser = takeWhileP (Just "Jet direction") (`elem` "<>") <* eol <* eof

rocks :: [Coord -> Rock]
rocks = map ((\s (x,y) -> S.map (\(dx,dy) -> (x+dx,y+dy)) s) . S.fromAscList)
            [ [(dx,0) | dx <- [0..3]] -- ^ "-" shape
            , [(0,1)]
           <> [(1,dy) | dy <- [0..2]]
           <> [(2,1)]                 -- ^ "+" shape
            , [(dx,0) | dx <- [0..2]]
           <> [(2,dy) | dy <- [1..2]] -- ^ reverse "L" shape
            , [(0,dy) | dy <- [0..3]] -- ^ "|" shape
            , [(dx,dy) | dx <- [0..1]
                       , dy <- [0..1]
              ]                       -- ^ square shape
            ]

push :: Char -> Int -> Int
push jet x | jet == '<' = max 0 (x - 1)
           | jet == '>' = min (7-1) (x + 1)

inBounds :: Rock -> Bool
inBounds cs | let xs = S.map fst cs
            = 0 <= S.findMin xs && S.findMax xs < 7

dropRocks :: [Coord -> Rock] -> JetPattern -> Int -> Rock -> (Int, Rock)
dropRocks [] _ pushes tower = (pushes, tower)
dropRocks (rock:rocks) jetPattern pushes tower
  = (\(jetPattern', pushes', tower') ->
      dropRocks rocks jetPattern' pushes' tower'
    )
  $ (foldr (\y more x (j:jP) ps ->
             let pushedX = push j x
                 pushedRock = rock (pushedX,y)
                 x' | S.disjoint tower pushedRock
                    , inBounds pushedRock
                    = pushedX
                    | otherwise = x
                 droppedY = y - 1
                 droppedRock = rock (x', droppedY)
                 (jP',ps',tower') | not (S.disjoint tower droppedRock)
                                  = (jP, ps + 1, rock (x', y) `S.union` tower)
                                  | otherwise = more x' jP (ps + 1)
              in (jP', ps', tower')
           )
           (\x (j:jP) ps -> let pushedX = push j x
                                pushedRock = rock (pushedX,0)
                                rock' | inBounds pushedRock = pushedRock
                                      | otherwise = rock (x,0)
                             in (jP, ps + 1, rock' `S.union` tower)
           )
           ([startY,startY-1..1])
           2
           jetPattern
           pushes
    )
  where
    startY = 3 + height tower

fall :: Int -> Input -> (Int, Rock)
fall nrRocks jetPattern = dropRocks (take nrRocks (cycle rocks))
                                    (cycle jetPattern)
                                    0
                                    S.empty

height :: Rock -> Int
height tower = case S.lookupMax (S.map snd tower) of
                 Nothing -> 0
                 Just y -> y + 1

render :: Rock -> RockString
render tower = RS . ('\n':) . intercalate "\n"
             $ [map (\x -> token (x,y)) [0..7-1] | y <- [maxY, maxY-1..0]]
  where
    maxY = S.findMax . S.map snd $ tower
    token c | c `S.member` tower = '#'
            | otherwise = '.'

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = height . snd . fall 2022 <$> input
  printAnswer "Tower height after 2022 rocks have fallen: " answer

periodAfterPrefix :: Eq a => [a] -> ([a], [a])
periodAfterPrefix as
  = foldr (\oneRep next ->
            let (pR:potentialRepetition) = reverse (take oneRep as)
                mPrefixPeriod
                  = foldr (\periodLength next ->
                            let (potentialPeriod, rest)
                                  = splitAt periodLength
                                            (pR:potentialRepetition)
                                (potentialRep, potentialPrefix)
                                  = splitAt periodLength rest
                                mPrefixPeriod
                                  | potentialPeriod == potentialRep
                                  = Just ( reverse potentialPrefix
                                         , reverse potentialPeriod
                                         )
                                  | otherwise = next
                             in mPrefixPeriod
                          )
                          Nothing
                          (map (+1) (elemIndices pR potentialRepetition))
                prefixPeriod | Just (pre, per) <- mPrefixPeriod = (pre, per)
                             | otherwise = next
             in prefixPeriod
          )
          (error "Not a periodic sequence")
          [2..]

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = (\jetPattern ->
                 (\(prefix, period) ->
                   let prefixRocks = 5 * length prefix
                       periodRocks = 5 * length period
                       onePeriodHeight = height (snd (fall ( prefixRocks
                                                           + periodRocks
                                                           )
                                                           jetPattern
                                                     )
                                                )
                       twoPeriodHeight = height (snd (fall ( prefixRocks
                                                           + 2 * periodRocks
                                                           )
                                                           jetPattern
                                                     )
                                                )
                       periodHeight = twoPeriodHeight - onePeriodHeight
                       prefixHeight = onePeriodHeight - periodHeight
                       (repetitions, remainder) = (1000000000000 - prefixRocks)
                                        `quotRem` periodRocks
                       remainderHeight = height (snd (fall ( prefixRocks
                                                           + periodRocks
                                                           + remainder
                                                           )
                                                           jetPattern
                                                     )
                                                )
                                       - prefixHeight
                                       - periodHeight
                    in prefixHeight + repetitions * periodHeight
                     + remainderHeight
                 )
                 . periodAfterPrefix
                 . map (\rs -> (\(pushes,tower) ->
                                 pushes `rem` (length jetPattern)
                               )
                               (fall rs jetPattern)
                       )
                 $ [5,10..]

               ) <$> input
  printAnswer "Tower height after 1000000000000 rocks have fallen: " answer

main :: IO ()
main = do
  let day = "Day 17: Pyroclastic Flow"
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
