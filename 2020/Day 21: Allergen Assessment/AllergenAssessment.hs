module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AoC

import Data.Char (isLower)
import qualified Data.Map as M
import qualified Data.Set as S

data Food = Food (S.Set String) (S.Set String)

word :: Parser String
word = takeWhile1P (Just "Letter") isLower

food :: Parser Food
food = do
  ingredients <- sepEndBy word hspace
  allergens <- between (char '(') (char ')')
    (do lexeme (string "contains")
        sepBy word (lexeme (char ','))
    )
  pure $ Food (S.fromList ingredients) (S.fromList allergens)

foods :: Parser [Food]
foods = sepEndBy food eol <* eof

allergenless :: [Food] -> S.Set String
allergenless foods = allIngredients S.\\ allergenic
  where
    allIngredients = foldr (\(Food ingredients _) rest -> ingredients <> rest)
                           S.empty
                           foods
    allergenMap =
      foldr (\(Food ingredients allergens) m ->
              S.foldr (\allergen m' ->
                        M.insertWith (S.intersection) allergen ingredients m'
                      )
                      m
                      allergens
            )
            M.empty
            foods
    allergenic = M.foldr (<>) S.empty allergenMap

countUsage :: S.Set String -> [Food] -> Integer
countUsage ingredients foods =
  foldr (\(Food is _) rest ->
          fromIntegral (S.size (S.intersection is ingredients)) + rest)
        0
        foods

part1 :: Parsed [Food] -> IO ()
part1 input = do
  let answer = (\foods -> countUsage (allergenless foods) foods) <$> input
  printAnswer "Times allergenless ingredients appear: " answer

part2 :: Parsed [Food] -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "Not an answer: " answer

main :: IO ()
main = do
  let day = "Day 21: Allergen Assessment"
  let parser = foods
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
