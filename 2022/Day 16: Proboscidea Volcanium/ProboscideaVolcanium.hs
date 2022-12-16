module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Map as M
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

maximumPressure :: Int -> Input -> Int
maximumPressure time valves
  = optimalPath valves valveDistances ("AA", time, brokenValves, 0)
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
  let answer = maximumPressure 30 <$> input
  printAnswer "Most releasable pressure: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const "P" <$> input
  printAnswer "No answer yet: " answer

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
