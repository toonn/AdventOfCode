module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S

import AoC

type Vertex = String
type FlowRate = Int
type ValveGraph = M.Map Vertex (FlowRate, [Vertex])
type Distances = M.Map Vertex (M.Map Vertex Int)

type Input = ValveGraph

name :: Parser Vertex
name = takeWhileP (Just "Valve name") (`elem` ['A'..'Z'])

valve :: Parser (Vertex, (FlowRate, [Vertex]))
valve = do
  lexeme (string "Valve")
  vertex <- name
  hspace
  lexeme (string "has flow rate=")
  flowRate <- integer
  lexeme ( string "; tunnels lead to valves"
       <|> string "; tunnel leads to valve"
         )
  edges <- sepBy name (lexeme (char ','))
  pure (vertex, (flowRate, edges))


parser :: Parser Input
parser = M.fromList <$> manyTill (valve <* eol) eof

distances :: Distances -> Distances
distances ds | ds == ds' = ds
             | otherwise = distances ds'
  where
    ds' = M.mapWithKey
            (\from tos ->
              M.delete from
                       (M.foldrWithKey (\to d tos' ->
                                         M.unionWith min
                                                     tos'
                                                     (M.map (+d) (ds M.! to))
                                       )
                                       tos
                                       tos
                       )
            )
            ds

optimalPath :: ValveGraph
            -> Distances
            -> (Vertex, Int, S.Set Vertex, Int)
            -> Int
optimalPath valves
            valveDistances
            (valve, timeLeft, openValves, pressureToBeReleased)
  | timeLeft == 0 = pressureToBeReleased
  | otherwise = maximum
              . (pressureToBeReleased:)
              . map (optimalPath valves valveDistances)
              $ candidates
  where
    reachableValves = M.withoutKeys (valveDistances M.! valve) openValves
    cs = M.foldrWithKey (\to d ->
                          let timeLeft' = timeLeft - d - 1
                              flowRate = fst (valves M.! to)
                              next
                                | timeLeft' >= 0 = ( ( to
                                                     , timeLeft'
                                                     , S.insert to openValves
                                                     , pressureToBeReleased
                                                     + flowRate * timeLeft'
                                                     )
                                                   :
                                                   )
                                | otherwise = id
                           in next
                        )
                        []
                        reachableValves
    flowRate = fst (valves M.! valve)
    timeLeft' = timeLeft - 1
    candidates | valve `S.member` openValves = cs
               | otherwise = ( valve
                             , timeLeft'
                             , S.insert valve openValves
                             , pressureToBeReleased + flowRate * timeLeft'
                             )
                           : cs

maximumPressure :: Int
                -> ( ValveGraph -> Distances -> (Vertex, Int, S.Set Vertex, Int)
                   -> Int
                   )
                -> Input
                -> Int
maximumPressure time optimise valves
  = optimise valves valveDistances ("AA", time, brokenValves, 0)
  where
    brokenValves = M.keysSet . M.filter ((== 0) . fst) $ valves
    valveDistances = distances
                       (M.foldrWithKey
                         (\from (_,es) ds ->
                           foldr (\to ->
                                   M.insertWith M.union from (M.singleton to 1)
                                 )
                                 ds
                                 es
                         )
                         M.empty
                         valves
                       )

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = maximumPressure 30 optimalPath <$> input
  printAnswer "Most releasable pressure: " answer

optimalTogether :: ValveGraph
                -> Distances
                -> (Vertex, Int, Vertex, Int, Int, S.Set Vertex, Int)
                -> Int
optimalTogether valves
                valveDistances
                ( valve1
                , time1
                , valve2
                , time2
                , timeLeft
                , openValves
                , pressureToBeReleased
                )
  | timeLeft == 0 = pressureToBeReleased
  | time1 == 0, time2 == 0 = pressureToBeReleased
  | time1 < timeLeft && time2 < timeLeft
  = optimalTogether valves
                    valveDistances
                    ( valve1
                    , time1
                    , valve2
                    , time2
                    , max time1 time2
                    , openValves
                    , pressureToBeReleased
                    )
  | otherwise = maximum
              . (pressureToBeReleased:)
              . map (optimalTogether valves valveDistances)
              $ candidates
  where
    idealizedPressureRelease = (timeLeft - 1)
                             * (M.foldr ((+) . fst)
                                        0
                                        (M.withoutKeys valves openValves)
                               )
    reachableValves1 = M.withoutKeys (valveDistances M.! valve1) openValves
    reachableValves2 = M.withoutKeys (valveDistances M.! valve2) openValves
    candidatePressures reachableValves v
        = M.foldrWithKey (\to d ->
                           let timeLeft' = timeLeft - d - 1
                               flowRate = fst (valves M.! to)
                               next | timeLeft' >= 0 = ( ( to
                                                         , timeLeft'
                                                         , timeLeft' * flowRate
                                                         )
                                                       :
                                                       )
                                    | otherwise = id
                            in next
                         )
                         []
                         reachableValves
    cPs1 | time1 == timeLeft
         = case candidatePressures reachableValves1 valve1 of
             [] -> [(valve1, 0, 0)]
             cPs -> cPs
         | otherwise = [(valve1, time1, 0)]
    cPs2 | time2 == timeLeft
         = case candidatePressures reachableValves2 valve2 of
             [] -> [(valve2, 0, 0)]
             cPs -> cPs
         | otherwise = [(valve2, time2, 0)]
    cs = concatMap
           (\(v1,t1,p1) ->
             mapMaybe (\(v2,t2,p2) ->
                        let c | v1 == v2 = Nothing
                              | valve1 == v1, time1 == t1
                              , valve2 == v2, time2 == t2
                              = Nothing
                              | otherwise
                              = Just ( v1
                                     , t1
                                     , v2
                                     , t2
                                     , timeLeft
                                     , S.insert v1 (S.insert v2 openValves)
                                     , pressureToBeReleased + p1 + p2
                                     )
                         in c
                      )
                      cPs2
           )
           cPs1
    maxCandidatePressure = maximum
                         . map (\(_,_,_,_,_,_,p) -> p)
                         $ cs
    candidates = filter (\(_,_,_,_,_,_,p) ->
                          p + idealizedPressureRelease > maxCandidatePressure
                        )
                        cs

optimalPathTogether :: ValveGraph
                    -> Distances
                    -> (Vertex, Int, S.Set Vertex, Int)
                    -> Int
optimalPathTogether valves
                    valveDistances
                    (start, timeLeft, openValves, pressureToBeReleased)
  = optimalTogether valves
                    valveDistances
                    ( start
                    , timeLeft'
                    , start
                    , timeLeft'
                    , timeLeft'
                    , openValves
                    , pressureToBeReleased
                    )
  where
    timeLeft' = timeLeft - 4

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = maximumPressure 30 optimalPathTogether <$> input
  printAnswer "Most releasable pressure when working with an elephant: " answer

main :: IO ()
main = do
  let day = "Day 16: Proboscidea Volcanium"
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
