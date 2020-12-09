module Main where

import Criterion.Main
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Data.Void (Void)
import System.FilePath ((</>))
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Paths_AoC2020

type Parser = Parsec Void String
type Parsed a = Either (ParseErrorBundle String Void) a

type Numbers = [Integer]
type Queue a = (Seq.Seq a, S.Set a)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space hspace1 empty empty)

integer :: Parser Integer
integer = lexeme L.decimal

xMAS :: Parser Numbers
xMAS = sepEndBy integer eol

readInput :: String -> IO (Parsed Numbers)
readInput day = do
  inputFile <- getDataFileName (day </> "input.txt")
  parse xMAS inputFile
    <$> readFile inputFile

printAnswer :: Show a => String -> Parsed a -> IO ()
printAnswer question answer =
  either (putStrLn . errorBundlePretty)
         (putStrLn . (question <>) . show)
         answer

push :: Ord a => a -> Queue a -> Queue a
push a (s Seq.:<| seq, set) = (seq Seq.|> a, S.insert a (S.delete s set))

sumsTo :: S.Set Integer -> Integer -> Integer -> Bool -> Bool
sumsTo summands nr summand next =
  let s = nr - summand
   in case s /= summand && s `S.member` summands of
        True -> True
        False -> next

valid :: Queue Integer -> Integer -> Bool
valid (_, summands) nr = S.foldr (sumsTo summands nr) False summands

firstNonSum :: Numbers -> Integer
firstNonSum nrs = foldr crease
                        (\_ -> error "No invalid Nr")
                        body
                        (Seq.fromList preamble, S.fromList preamble)
  where
    (preamble, body) = L.splitAt 25 nrs
    crease :: Integer -> (Queue Integer -> Integer) ->  Queue Integer -> Integer
    crease nr next summands
      | valid summands nr = next (push nr summands)
      | otherwise = nr

part1 :: Parsed Numbers -> IO (Parsed Integer)
part1 input = do
  let answer = firstNonSum <$> input
  printAnswer "First invalid Nr: " answer
  return answer

findSummingSequence :: Integer
                    -> Integer
                    -> (Integer -> Seq.Seq Integer -> Seq.Seq Integer)
                    -> Integer
                    -> Seq.Seq Integer
                    -> Seq.Seq Integer
findSummingSequence p1 n next sum seq = case sum' `compare` p1 of
  LT -> next sum' seq'
  EQ -> seq'
  GT -> case dropSummands sum' seq' of
          (sum'', seq'') | sum'' == p1 -> seq''
                         | otherwise -> next sum'' seq''
  where
    sum' = sum + n
    seq' = seq Seq.|> n
    dropSummands sum (x Seq.:<| xs) | sum - x <= p1 = (sum - x, xs)
                                    | otherwise = dropSummands (sum - x) xs

minMax :: Ord a => Seq.Seq a -> (a,a)
minMax Seq.Empty = error "Min and max of an empty sequence don't exist."
minMax (x Seq.:<| xs) = foldr envelop id xs (x,x)
  where
    envelop y next (min, max) | y < min = next (y, max)
                              | y > max = next (min, y)
                              | otherwise = next (min, max)

weakness :: Integer -> Numbers -> Integer
weakness p1 nrs = (uncurry (+)) . minMax $
  foldr (findSummingSequence p1)
        (\_ _ -> error "No encryption weakness")
        nrs
        0
        Seq.Empty

part2 :: Parsed Integer -> Parsed Numbers -> IO ()
part2 p1 input = do
  let answer = weakness <$> p1 <*> input
  printAnswer "Encryption weakness: " answer

main :: IO ()
main = do
  let day =  "Day 9: Encoding Error"
  input <- readInput day
  putStrLn ""
  p1 <- part1 input
  part2 p1 input
  putStrLn ""
  defaultMain [
      bgroup "AoC"
        [ bench "Part 1" $ nfIO (silence $ readInput day >>= part1)
        , bench "Part 2" $ nfIO (silence $ readInput day >>= part2 p1)
        ]
    ]
