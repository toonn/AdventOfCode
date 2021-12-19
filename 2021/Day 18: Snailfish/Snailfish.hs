module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec hiding (chunk)
import Text.Megaparsec.Char

import Data.List (delete)

import AoC

data SnailfishNumber = R Int | P SnailfishNumber SnailfishNumber
       deriving Eq

instance Show SnailfishNumber where
  show (R n) = show n
  show (P a b) = mconcat ["[", show a, ",", show b, "]"]

type Homework = [SnailfishNumber]

snailfishNumber :: Parser SnailfishNumber
snailfishNumber = (R <$> integer)
              <|> (do char '['
                      a <- snailfishNumber
                      char ','
                      b <- snailfishNumber
                      char ']'
                      pure (P a b)
                  )

parser :: Parser Homework
parser = manyTill (snailfishNumber <* eol) eof

addLeftmost :: Maybe Int -> SnailfishNumber -> (Maybe Int, SnailfishNumber)
addLeftmost Nothing n = (Nothing, n)
addLeftmost (Just r) (R i) = (Nothing, R (i + r))
addLeftmost (Just r) (P a b) = case addLeftmost (Just r) a of
  (Nothing, a') -> (Nothing, P a' b)
  (Just _, _) -> case addLeftmost (Just r) b of
    (Nothing, b') -> (Nothing, P a b')
    (Just _, _) -> (Just r, P a b)

addRightmost :: Maybe Int -> SnailfishNumber -> (Maybe Int, SnailfishNumber)
addRightmost Nothing n = (Nothing, n)
addRightmost (Just l) (R i) = (Nothing, R (i + l))
addRightmost (Just l) (P a b) = case addRightmost (Just l) b of
  (Nothing, b') -> (Nothing, P a b')
  (Just _, _) -> case addRightmost (Just l) a of
    (Nothing, a') -> (Nothing, P a' b)
    (Just _, _) -> (Just l, P a b)

explode :: SnailfishNumber -> Maybe SnailfishNumber
explode =  (fmap (\(_, _, n) -> n)) . go 0
  where
    go :: Int -> SnailfishNumber
       -> Maybe (Maybe Int, Maybe Int, SnailfishNumber)
    go i n | i < 4 = case n of
                       R _ -> Nothing
                       P a b -> case (go (i+1) a, go (i+1) b) of
                         (Just (l, r, a'), _) -> case addLeftmost r b of
                           (r', b') -> Just (l, r', P a' b')
                         (Nothing, Just (l, r, b')) -> case addRightmost l a of
                           (l', a') -> Just (l', r, P a' b')
                         (Nothing, Nothing) -> Nothing
           | otherwise = case n of
             R _ -> Nothing
             P (P a b) c -> case (go i a, go i b) of
               (Just (l, r, a'), _) -> case addLeftmost r b of
                 (r', b') -> Just (l, r', P (P a' b') c)
               (Nothing, Just (l, r, b')) ->
                 case (addRightmost l a, addLeftmost r c) of
                   ((l',a'),(r',c')) -> Just (l', r', P (P a' b') c')
               (Nothing, Nothing) -> case go i c of
                 Just (l, r, c') -> case addRightmost l (P a b) of
                   (l', ab') -> Just (l', r, P ab' c')
                 Nothing -> Nothing
             P (R a) (P b c) -> case (go i b, go i c) of
               (Just (l, r, b'), _) ->
                 case (addRightmost l (R a), addRightmost l b) of
                   ((l', a'), (r', c')) -> Just (l', r', P a' (P b' c'))
               (Nothing, Just (l, r, c')) -> case addRightmost l b of
                 (l', b') -> Just (l', r, P (R a) (P b' c'))
               (Nothing, Nothing) -> Nothing
             P (R a) (R b) -> Just (Just a, Just b, R 0)

split :: SnailfishNumber -> Maybe SnailfishNumber
split (R i) | i < 10 = Nothing
            | otherwise = case i `quotRem` 2 of
                            (q, r) -> Just (P (R q) (R (q + r)))
split (P a b) = case (split a, split b) of
                  (Just a', _) -> Just (P a' b)
                  (Nothing, Just b') -> Just (P a b')
                  (Nothing, Nothing) -> Nothing

reduce :: SnailfishNumber -> SnailfishNumber
reduce n | Just n' <- explode n = reduce n'
         | Just n' <- split n = reduce n'
         | otherwise = n

add :: SnailfishNumber -> SnailfishNumber -> SnailfishNumber
add = (reduce .) . P

magnitude :: SnailfishNumber -> Int
magnitude (R i) = i
magnitude (P a b) = 3 * magnitude a + 2 * magnitude b

part1 :: Parsed Homework -> IO ()
part1 input = do
  let answer = magnitude
             . (\homework -> foldr (\n next sum -> next (add sum n))
                                   id
                                   (tail homework)
                                   (head homework)
               )
           <$> input
  printAnswer "Final sum magnitude: " answer

part2 :: Parsed Homework -> IO ()
part2 input = do
  let answer = maximum
             . map (magnitude . uncurry add)
             . (\homework ->
                 concatMap (\a ->
                             map (\b -> (a,b))
                                 (delete a homework)
                           )
                           homework
               )
           <$> input
  printAnswer "Largest magnitude of two different numbers: " answer

main :: IO ()
main = do
  let day = "Day 18: Snailfish"
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
