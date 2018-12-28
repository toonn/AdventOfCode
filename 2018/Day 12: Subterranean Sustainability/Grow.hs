#! /usr/bin/env nix-shell
#! nix-shell -i runghc
#! nix-shell -p "haskellPackages.ghcWithPackages (ps: [ ps.containers ])"

{-# LANGUAGE BangPatterns #-}

module Grow where

import Data.Char
import Data.List (zipWith5, foldl')
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Text.ParserCombinators.ReadP

type Generation = (Int,[Bool])
type Transitions = Bool -> Bool -> Bool -> Bool -> Bool -> Bool

showGen :: [Bool] -> String
showGen = map (\x -> if x then '#' else '.')

dotHash :: ReadP Bool
dotHash = (False <$ char '.') +++ (True <$ char '#')

whiteSpace :: ReadP ()
whiteSpace = () <$ munch1 isSpace

dotHashes :: ReadP [Bool]
dotHashes = manyTill dotHash whiteSpace

parseInitial :: ReadP Generation
parseInitial = optional whiteSpace >>
  string "initial state:" >>
  whiteSpace >>
  dotHashes >>= \gen ->
  return (0, gen)

parseTransitions :: ReadP Transitions
parseTransitions =
  toIndex <$> manyTill parseTransition (optional whiteSpace >> eof)
  where
    tup [a,b,c,d,e] = (a,b,c,d,e)
    parseTransition = optional whiteSpace >>
                      dotHashes >>= \key ->
                      optional whiteSpace >> string "=>" >> whiteSpace >>
                      dotHash >>= \val ->
                      return (tup key, val)
    toIndex' m [] = \a b c d e -> M.findWithDefault False (a, b, c, d, e) m
    toIndex' m ((k,v):kvs) = toIndex' (M.insert k v m) kvs
    toIndex = toIndex' M.empty

parseStateRules :: String -> (Generation, Transitions)
parseStateRules = fst . head . readP_to_S
  (parseInitial >>= \il ->
   parseTransitions >>= \ts ->
   return (il, ts))

nextGen :: Transitions -> Generation -> Generation
nextGen trans (lEdge, gen) = chop (lEdge - 3) (dropTail (zipWith5 trans
  (expandLeft 5 gen) (expandLeft 4 gen <> dots) (expandLeft 3 gen <> dots)
  (expandLeft 2 gen <> dots) (expandLeft 1 gen <> dots)))
  where
    expandLeft 0 = id
    expandLeft x | x > 0 = (False:) . expandLeft (x-1)
    dots = False:dots
    fromFalse False [] = []
    fromFalse x xs = x:xs
    dropTail = foldr fromFalse []
    chop x (False:xs) = chop (x+1) xs
    chop x xs = (x, xs)

bigStepGen :: Int -> Transitions -> Generation -> Generation
bigStepGen 0 _ g = g
bigStepGen x trans g
  | x > 0 && snd g == snd nextG = (lEdge, snd g)
  | x > 0                       = bigStepGen (x-1) trans nextG
  where
    nextG = nextGen trans g
    lEdge = fst g + x * (fst nextG - fst g)

livePotChecksum :: Int -> (Generation, Transitions) -> Int
livePotChecksum gens (g0, trans) = snd $ foldl' sumPots (lEdge, 0) g20
  where
    (lEdge, g20) = bigStepGen gens trans g0
    sumPots (ix, tot) False = (ix + 1, tot)
    sumPots (ix, tot) True = (ix + 1, tot + ix)

type Generation' = (Int, S.Set Int, Int)
type Transitions' = Seq.Seq Bool -> Bool

showGen' :: (Int, S.Set Int, Int) -> String
showGen' (min, g, max) = map (\x -> if x `S.member` g then '#' else '.')
                             [min..max]

parseInitial' :: ReadP Generation'
parseInitial' = optional whiteSpace >>
  string "initial state:" >>
  whiteSpace >>
  dotHashes >>= \gen ->
  return $ case foldl' (\(cIx, (iXs, maxIx)) plant ->
                         (cIx + 1, if plant then (S.insert cIx iXs, cIx)
                                            else (iXs, maxIx)))
                      (0, (S.empty, 0))
                      gen of
             (_, (iXs, maxIx)) -> (0, iXs, maxIx)

parseTransitions' :: ReadP Transitions'
parseTransitions' =
  toIndex <$> manyTill parseTransition (optional whiteSpace >> eof)
  where
    parseTransition = optional whiteSpace >>
                      dotHashes >>= \key ->
                      optional whiteSpace >> string "=>" >> whiteSpace >>
                      dotHash >>= \val ->
                      return (Seq.fromList key, val)
    toIndex' m [] = \k -> M.findWithDefault False k m --(m M.!)
    toIndex' m ((k,v):kvs) = toIndex' (M.insert k v m) kvs
    toIndex = toIndex' M.empty

parseStateRules' :: String -> (Generation', Transitions')
parseStateRules' = fst . head . readP_to_S
  (parseInitial' >>= \il ->
   parseTransitions' >>= \ts ->
   return (il, ts))

nextGen' :: Transitions' -> Generation' -> Generation'
nextGen' trans (min, gen, max) = snd $ foldl' step
  (Seq.replicate 5 False, (min, gen, max))
  [min..max+4]
  where
    ins pot (min, gen, max) | pot < min = (pot, S.insert pot gen, max)
                            | pot > max = (min, S.insert pot gen, pot)
                            | otherwise = (min, S.insert pot gen, max)
    del pot (min, gen, max)
      | pot == min = (fromJust $ S.lookupGT min gen, S.delete min gen, max)
      | pot == max = (min, S.delete max gen, fromJust $ S.lookupLT max gen)
      | otherwise  = (min, S.delete pot gen, max)
    step (_ Seq.:<| seqTail, (!min, !gen, !max)) iX
      | shouldBeInSet && not inSet =
        (currentSeq, ins middleIx (min, gen, max))
      | not shouldBeInSet && inSet =
        (currentSeq, del middleIx (min, gen, max))
      | otherwise                  = (currentSeq, (min, gen, max))
      where
        middleIx = iX - 2
        inSet = middleIx `S.member` gen
        currentSeq = seqTail Seq.|> (iX `S.member` gen)
        shouldBeInSet = trans currentSeq

-- Slower than the list approach o.O
-- The stop condition doesn't actually work and is very hard to implement with
-- this representation.
bigStepGen' :: Int -> Transitions' -> Generation' -> Generation'
bigStepGen' 0 _ g = g
bigStepGen' x trans g | x > 0 && g == nextG = g
                      | x > 0               = bigStepGen' (x-1) trans nextG
  where
    nextG = nextGen' trans g

livePotChecksum' :: Int -> (Generation', Transitions') -> Int
livePotChecksum' gens (g0, trans) = sum g5e10
  where
    (_, g5e10, _) = bigStepGen' gens trans g0

main :: IO ()
main = do
  stateDesc <- readFile "./input"
  let start = parseStateRules stateDesc
  let start' = parseStateRules' stateDesc
  report "Sum of live pots after 20 generations: "
         (show . livePotChecksum 20)
         start
  report "Sum of live pots after 50000000000 generations: "
         (show . livePotChecksum 50000000000)
         start
  where
    report msg f = putStrLn . (msg <>) . f
