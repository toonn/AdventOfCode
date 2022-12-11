module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.IntMap as IM
import Data.List (sortBy)
import Text.Parser.Combinators (chainl1)

import AoC

type Worry = Int
type WorryRule = Worry -> Worry
type MonKey = Int {- Confusing due to subtle capitalization difference but
                     deemed necessary for the bad pun.
                  -}
type ThrowRule = Worry -> MonKey
type Monkey = (WorryRule, ThrowRule, [Worry])

data Expr = Var
          | Val Int
          | Mul Expr Expr
          | Add Expr Expr

type Input = ([Int], IM.IntMap Monkey)

interpretWorry :: Expr -> WorryRule
interpretWorry Var x = x
interpretWorry (Val v) _ = v
interpretWorry (Mul l r) x = interpretWorry l x * interpretWorry r x
interpretWorry (Add l r) x = interpretWorry l x + interpretWorry r x

parseWorryRule :: Parser WorryRule
parseWorryRule = do
  hspace
  lexeme (string "Operation:")
  lexeme (string "new =")
  expr <- chainl1 (chainl1 ( (lexeme (string "old") *> pure Var)
                         <|> (Val <$> integer)
                           )
                           (Mul <$ lexeme (char '*'))
                  )
                  (Add <$ lexeme (char '+'))
  pure (interpretWorry expr)

parseThrowRule :: Parser (Int, ThrowRule)
parseThrowRule = do
  hspace
  lexeme (string "Test:")
  lexeme (string "divisible by")
  divisor <- integer
  eol
  ifThrowTo
  trueMonKey <- integer
  eol
  ifThrowTo
  falseMonKey <- integer
  pure ( divisor
       , \worry -> if worry `rem` divisor == 0 then trueMonKey else falseMonKey
       )
  where ifThrowTo = hspace
                 *> lexeme (string "If")
                 *> (string "true" <|> string "false")
                 *> lexeme (string ":")
                 *> lexeme (string "throw to monkey")

monkey :: Parser (Int, (MonKey, Monkey))
monkey = do
  key <- lexeme (string "Monkey") *> integer <* char ':'
  eol
  hspace
  items <- lexeme (string "Starting items:")
           *> sepBy integer (char ',' *> hspace)
  eol
  worryRule <- parseWorryRule
  eol
  (divisor, throwRule) <- parseThrowRule
  eol
  pure (divisor, (key, (worryRule, throwRule, items)))

parser :: Parser Input
parser = (\(divisors, ms) -> (divisors, IM.fromAscList ms)) . unzip
     <$> (sepBy monkey eol <* eof)

keepAwayRound :: (Worry -> Worry)
              -> IM.IntMap (WorryRule, ThrowRule)
              -> IM.IntMap (Int, [Worry])
              -> IM.IntMap (Int, [Worry])
keepAwayRound worryReduction monkeys monkeyItems
  = IM.foldrWithKey
      (\k (nr,items) nextMonkey ms ->
        let items' = items <> snd (ms IM.! k)
            nr' = nr + length items'
         in nextMonkey (foldr (\w nextItem ms' ->
                                let (worryRule, throwRule) = monkeys IM.! k
                                    w' = worryReduction (worryRule w)
                                    k' = throwRule w'
                                 in nextItem (IM.insertWith (\(_,ws') (n,ws) ->
                                                              (n,ws <> ws')
                                                            )
                                                            k'
                                                            (0, [w'])
                                                            ms'
                                             )
                              )
                              id
                              items'
                              (IM.insert k (nr',[]) ms)
                       )
      )
      id
      monkeyItems
      (IM.map (\(n,ws) -> (n,[])) monkeyItems)

nTimes :: Int -> (a -> a) -> a -> a
nTimes rounds = foldr (.) id . replicate rounds

-- monkeyBusiness :: (Worry -> Worry) -> Int -> Input -> Int
monkeyBusiness worryReduction rounds monkeys
  = product
  . take 2
  . sortBy (flip compare)
  . IM.foldr ((:) . fst)
             []
  $ nTimes rounds (keepAwayRound worryReduction monkeyRules) monkeyItems
    where
      monkeyRules = IM.map (\(wR,tR,_) -> (wR,tR)) monkeys
      monkeyItems = IM.map (\(_,_,is) -> (0,is)) monkeys

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = monkeyBusiness (`quot` 3) 20 . snd <$> input
  printAnswer "Monkey business level after 20 rounds: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = (\(divisors,monkeys) ->
                 monkeyBusiness (`mod` (product divisors)) 10000 monkeys
               )
           <$> input
  printAnswer "Level after 10000 rounds without worry reduction: " answer

main :: IO ()
main = do
  let day = "Day 11: Monkey in the Middle"
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
