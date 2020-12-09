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

part1 :: Parsed Numbers -> IO ()
part1 input = do
  let answer = firstNonSum <$> input
  printAnswer "First invalid Nr: " answer

part2 :: Parsed Numbers -> IO ()
part2 input = do
  let answer = const "P" <$> input
  printAnswer "Not an answer: " answer

main :: IO ()
main = do
  let day =  "Day 9: Encoding Error"
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
