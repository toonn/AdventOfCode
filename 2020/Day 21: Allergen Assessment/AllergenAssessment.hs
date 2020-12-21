module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AoC

import Data.Char (isLower)
import Data.List (intercalate)
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

allergenMap :: [Food] -> M.Map String (S.Set String)
allergenMap foods =
  foldr (\(Food ingredients allergens) m ->
          S.foldr (\allergen m' ->
                    M.insertWith (S.intersection) allergen ingredients m'
                  )
                  m
                  allergens
        )
        M.empty
        foods

allergenless :: [Food] -> S.Set String
allergenless foods = allIngredients S.\\ allergenic
  where
    allIngredients = foldr (\(Food ingredients _) rest -> ingredients <> rest)
                           S.empty
                           foods
    allergenic = M.foldr (<>) S.empty (allergenMap foods)

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

singleOut :: M.Map String String -> M.Map String (S.Set String)
          -> M.Map String String
singleOut seen allergens
  | M.null allergens = seen
  | otherwise = singleOut (M.insert allergen ingredient seen)
                          (M.map (S.delete ingredient)
                                 (M.delete allergen allergens)
                          )
  where
    (allergen, ingredient) = M.foldrWithKey (\allergen is next ->
                                               if S.size is == 1
                                               then (allergen, S.findMin is)
                                               else next
                                            )
                                            (error "No unique ingredient")
                                            allergens

sortedIngredients :: M.Map String String -> [String]
sortedIngredients = map snd . M.toAscList

part2 :: Parsed [Food] -> IO ()
part2 input = do
  let answer = intercalate ","
             . sortedIngredients
             . singleOut M.empty
             . allergenMap
           <$> input
  printAnswer "Canonical dangerous ingredients list: " answer

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
