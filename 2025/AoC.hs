module AoC where

import Control.Arrow ((&&&), (***))
import Control.Monad (guard, join)
import qualified Data.Map as M
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Set as S
import Data.Void (Void)
import System.FilePath ((</>))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Paths_AoC2025

type Parser = Parsec Void String
type Parsed a = Either (ParseErrorBundle String Void) a

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space hspace1 empty empty)

integer :: Num a => Parser a
integer = lexeme L.decimal

signed :: Num a => Parser a -> Parser a
signed = L.signed (L.space empty empty empty)

readInput :: String -> Parser a -> IO (Parsed a)
readInput day parser = do
  inputFile <- getDataFileName (day </> "input.txt")
  parse parser inputFile
    <$> readFile inputFile

printAnswer :: Show a => String -> Parsed a -> IO ()
printAnswer question answer =
  either (putStrLn . errorBundlePretty)
         (putStrLn . (question <>) . show)
         answer

-- Parser for the common case of a grid of characters
characterGrid :: Parser [[Char]]
characterGrid = sepEndBy1 (takeWhile1P (Just "Any but a newline") (/= '\n')) eol

both :: (a -> b) -> (a,a) -> (b,b)
both = join (***)

dupe :: a -> (a,a)
dupe = id &&& id

type YX = (Int, Int)

foldYX :: [[a]] -> M.Map YX a
foldYX rows = M.fromAscList
            $ foldr (\row more y ->
                      foldr (\a more x -> ((y, x), a) : more (x + 1))
                            (const (more (y + 1)))
                            row
                            0
                    )
                    (const [])
                    rows
                    0

twoDDeltas :: Num a => [(a,a)]
twoDDeltas = do dy <- [-1,0,1]
                dx <- [-1,0,1]
                pure (dy,dx)

fourWayDeltas :: (Eq a, Num a) => [(a,a)]
fourWayDeltas = do (dy,dx) <- twoDDeltas
                   guard (abs dy /= abs dx)
                   pure (dy, dx)

deltaNeighbors :: [YX] -> YX -> [YX]
deltaNeighbors deltas (y,x) = do (dy,dx) <- deltas
                                 pure (y + dy, x + dx)

hamming :: (Eq a, Num d) => [a] -> [a] -> d
hamming as bs = fromIntegral . length . filter id $ zipWith (/=) as bs

manhattan :: Integral a => (a,a) -> (a,a) -> a
manhattan (x,y) (x',y') = abs (x - x') + abs (y - y')

nTimes :: Int -> (a -> a) -> a -> a
nTimes rounds = foldr (.) id . replicate rounds

-- | A* - Because it comes back every year.
--
-- Based on the [pseudocode on Wikipedia](
-- https://en.wikipedia.org/wiki/A*_search_algorithm#Pseudocode).
--
-- @since 2023.12.17.1
aStar :: (Ord state, Show state)
      => (state -> [state])
      -> (state -> state -> Int)
      -> (state -> Int)
      -> (state -> Bool)
      -> state
      -> Int
aStar neighbors distance heuristic isGoal start =
  go (M.singleton start 0) (PQ.singleton (heuristic start) start)
  where
    -- Can't provide a type signature without ScopedTypeVariables because state
    -- here should be the same as in the signature for aStar.
    -- go :: M.Map state Int -> PQ.MinPQueue Int state -> Int
    go shortestPaths ((_, current) PQ.:< openSet)
      | isGoal current = shortestPaths M.! current
      | current_sP <- shortestPaths M.! current
      = uncurry go
      $ foldr (\neighbor more (sPs, oS) ->
                let tentative_sP = current_sP + distance current neighbor
                    next | Just neighbor_sP <- shortestPaths M.!? neighbor
                         , tentative_sP >= neighbor_sP
                         = (sPs, oS)
                         | otherwise
                         = ( M.insert neighbor tentative_sP sPs
                           , -- The pseudocode avoids duplicating states in the
                             -- openSet. It shouldn't matter much, as long as
                             -- the duplicated work is small enough.
                             PQ.insert (tentative_sP + heuristic neighbor)
                                       neighbor
                                       oS
                           )
                 in more next
              )
              id
              (neighbors current)
              (shortestPaths, openSet)

-- | Transitive Closure of a Map from keys to monoids keys, representing a DAG
--
-- Based on repeated BFS.
transitiveClosure :: (Foldable m, Eq (m a), Monoid (m a), Ord a)
                  => M.Map a (m a) -> M.Map a (m a)
transitiveClosure m
  = let m' = M.map (foldMap (\k -> M.findWithDefault mempty k m)) m
        closure | m == m' = m
                | otherwise = transitiveClosure m'
     in closure
