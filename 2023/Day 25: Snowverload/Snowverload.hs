module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Data.Char (isLetter)
import qualified Data.Map as M
import qualified Data.Set as S

type Component = String
type Edge = S.Set Component
type Edges = M.Map Component (S.Set Component)

type Input = [(Component,[Component])]

componentID :: Parser Component
componentID = takeWhileP (Just "component") isLetter

edges :: Parser (Component,[Component])
edges = do
  comp <- componentID
  lexeme (char ':')
  cs <- sepBy componentID (char ' ')
  pure $ (comp,cs)

parser :: Parser Input
parser = sepEndBy1 edges eol <* eof

edgeMap :: Input -> Edges
edgeMap = foldr (\(c,cs) ->
                  M.unionWith (<>)
                              (M.fromList ( (c,S.fromList cs)
                                          : map (\c' -> (c',S.singleton c)) cs
                                          )
                              )
                )
                mempty

bidirBFS :: Edges -> Component -> Component -> [Component]
bidirBFS eM start goal = go (M.singleton start []) (M.singleton goal [])
  where
    step = M.foldrWithKey (\p ps ->
                            (M.fromSet (const (p:ps))
                                      (eM M.! p S.\\ (S.fromList ps))
                            <>
                            )
                          )
                          mempty

    shortestPath fs bs = (\(m, prevs) -> reverse prevs <> (m : (bs M.! m)))
                     <$> M.lookupMin (M.intersection fs bs)

    go forwards backwards =
      let fs = step forwards
          bs = step backwards
          sP | fs == forwards || bs == backwards = []
             | Just path <- shortestPath forwards bs = path
             | Just path <- shortestPath fs bs = path
             | otherwise = go fs bs
       in sP

sever :: Int -> Edges -> Edges
sever cuts eM = splitEs
  where
    Just (start, bs) = S.minView (M.keysSet eM)

    changeEdges change es p = foldr (\(f,t) more m ->
                                      more ( M.adjust (change t) f
                                           . M.adjust (change f) t
                                           $ m
                                           )
                                    )
                                    id
                                    (zip p (tail p))
                                    es

    removeEdges = changeEdges S.delete

    disjointPaths n es goal | p <- bidirBFS es start goal
                            = let r | n == 0, null p
                                    = Just []
                                    | n > 0
                                    = (p :) <$> disjointPaths (n - 1)
                                                              (removeEdges es p)
                                                              goal
                                    | otherwise
                                    = Nothing
                               in r

    addEdges = changeEdges S.insert

    findCut p es = let es' = addEdges es p
                    in foldr (\(f,t) next ->
                                let es'' = removeEdges es' [f,t]
                                    r | null (bidirBFS es'' f t) = es''
                                      | otherwise = next
                                 in r
                             )
                             (error "No valid cut")
                             (zip p (tail p))

    splitEs = S.foldr (\b next ->
                        let r | Just ps <- disjointPaths cuts eM b
                              , otherEs <- foldr (\p more es ->
                                                   more (removeEdges es p)
                                                 )
                                                 id
                                                 ps
                                                 eM
                              = foldr (\p more es ->
                                        more (findCut p es)
                                      )
                                      id
                                      ps
                                      otherEs
                              | otherwise
                              = next
                         in r
                      )
                      (error "Couldn't cut it")
                      bs

partitions :: Edges -> [S.Set Component]
partitions eM | Just ((c,cs), es) <- M.minViewWithKey eM
              , cs' <- closure es (S.insert c cs)
              = cs' : partitions (M.withoutKeys es cs')
              | otherwise
              = []
  where
    closure es cs = let cs' = cs <> mconcat (M.elems (M.restrictKeys es cs))
                        r | cs == cs' = cs
                          | otherwise = closure es cs'
                     in r

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = product . map length . partitions . sever 3 . edgeMap <$> input
  printAnswer "Product of group sizes: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 25: Snowverload"
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
