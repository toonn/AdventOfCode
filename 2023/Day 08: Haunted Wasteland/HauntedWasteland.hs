module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Map as M
import qualified Data.Set as S

import AoC

type Instructions = String
type Node = String

type Input = (Instructions, [(Node, (Node, Node))])

instructions :: Parser Instructions
instructions = takeWhile1P (Just "instruction") (`elem` "RL")

node :: Parser Node
node = takeP (Just "Label character") 3

edges :: Parser (Node, (Node, Node))
edges = do
  f <- lexeme node
  lexeme (char '=')
  es <- between (char '(')
                (char ')')
                ((,) <$> node <* (lexeme (char ',')) <*> node)
  pure (f, es)

parser :: Parser Input
parser = do
  is <- instructions
  eol
  eol
  es <- sepEndBy edges eol
  eof
  pure (is, es)

stepsToZZZ :: Input -> Int
stepsToZZZ (is, es) = let network = M.fromList es
                       in snd
                        . foldr (\i more (current, steps) ->
                                  let options = network M.! current
                                      next | i == 'L'  = fst options
                                           | otherwise = snd options
                                      steps' | current == "ZZZ"
                                             = (current, steps)
                                             | otherwise
                                             = more (next, steps + 1)
                                   in steps'
                                )
                                id
                                (cycle is)
                        $ ("AAA", 0)

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = stepsToZZZ <$> input
  printAnswer "Steps to reach ZZZ: " answer

go :: Char -> (Node, Node) -> Node
go 'L' = fst
go _   = snd

follow :: M.Map Node (Node, Node) -> Instructions -> Node -> (Node, S.Set Integer)
follow network instructions start =
  let z | 'Z' == last start = S.singleton 0
        | otherwise = S.empty
   in foldr (\i more (current, zs) step ->
              let next = go i (network M.! current)
                  step' = step + 1
                  zs' | 'Z' == last next = S.insert step' zs
                      | otherwise = zs
               in more (next, zs') step'
            )
            const
            instructions
            (start, z)
            0

period :: M.Map Node (Node, Node) -> Instructions -> Node
       -> (Integer, S.Set Integer, Integer, S.Set Integer)
period network instructions start =
  let nrInstructions = fromIntegral (length instructions)
   in foldr (\(current, zs) more repetitions zInitial seen ->
            let cycleInfo
                  | M.member current seen
                  , let (p, zPeriod) = seen M.! current
                  , let p' = p + 1
                  , let repetitions' = repetitions - p'
                  = ( repetitions'
                    , S.filter (<= repetitions' * nrInstructions) zInitial
                    , p'
                    , zPeriod
                    )
                  | otherwise
                  = let zCurrent | 'Z' == last current = S.singleton 0
                                 | otherwise = S.empty
                     in more
                         (repetitions + 1)
                         (S.union
                           zInitial
                           (S.map (+ ((repetitions - 1) * nrInstructions)) zs)
                         )
                         (M.insert
                           current
                           (0, zCurrent)
                           (M.map
                             (\(r, zPeriod) ->
                               let r' = r + 1
                                in ( r'
                                   , S.union zPeriod
                                             (S.map (+ r * nrInstructions) zs)
                                   )
                             )
                             seen
                           )
                         )
             in cycleInfo
          )
          (\r zInitial _ -> (r, zInitial, 0, S.empty))
          (iterate (follow network instructions . fst) (start, S.empty))
          0
          S.empty
          M.empty

mergePeriods :: Integer
             -> (Integer, S.Set Integer, Integer, S.Set Integer)
             -> (Integer, S.Set Integer, Integer, S.Set Integer)
             -> (Integer, S.Set Integer, Integer, S.Set Integer)
mergePeriods nrInstructions
             (untilCycle1, zInitial1, p1, zPeriod1)
             (untilCycle2, zInitial2, p2, zPeriod2) =
  let (untilCycle, zInitial, zPeriod1', zPeriod2')
          | untilCycle1 < untilCycle2
          , let uCD = untilCycle2 - untilCycle1
          , let (nrPeriods, shift) | (nrP, sP) <- uCD `quotRem` p1
                                   = let n | sP == 0 = 0
                                           | otherwise  = 1
                                      in (nrP + n, sP)
          , let zI = S.unions
                   . map (\uC -> S.filter (<= untilCycle2 * nrInstructions)
                               . S.map (\z -> z + uC * nrInstructions)
                               $ zPeriod1
                         )
                   $ [untilCycle1..untilCycle2 - 1]
          , let zP = S.map (+ shift * nrInstructions) zPeriod1
          = ( untilCycle2
            , zI
            , zP
            , zPeriod2
            )
          | otherwise
          , let uCD = untilCycle1 - untilCycle2
          , let (nrPeriods, shift) | (nrP, sP) <- uCD `quotRem` p2
                                   = let n | sP == 0 = 0
                                           | otherwise  = 1
                                      in (nrP + n, sP)
          , let zI = S.unions
                   . map (\uC -> S.filter (<= untilCycle1 * nrInstructions)
                               . S.map (\z -> z + uC * nrInstructions)
                               $ zPeriod2
                         )
                   $ [untilCycle2..untilCycle1 - 1]
          , let zP = S.map (+ shift * nrInstructions) zPeriod2
          = ( untilCycle1
            , zI
            , zPeriod1
            , zP
            )
      p = lcm p1 p2
      zPeriod1'' | let f1 = p `quot` p1
                 = S.unions
                 . map (\f -> S.filter (<= p * nrInstructions)
                            . S.map (+ (f * p1) * nrInstructions)
                            $ zPeriod1'
                       )
                 $ [0..f1]
      zPeriod2'' | let f2 = p `quot` p2
                 = S.unions
                 . map (\f -> S.filter (<= p * nrInstructions)
                            . S.map (+ (f * p2) * nrInstructions)
                            $ zPeriod2'
                       )
                 $ [0..f2]
      zPeriod = S.intersection zPeriod1'' zPeriod2''
   in (untilCycle, zInitial, p, zPeriod)

stepsToxxZ :: Input -> Integer
stepsToxxZ (is, es) = let network = M.fromList es
                          nrInstructions = fromIntegral (length is)
                       in {- (\(untilCycle, zInitial, _, zPeriod) ->
                            S.findMin ( zInitial
                                     <> (S.map (+ untilCycle * nrInstructions)
                                               zPeriod
                                        )
                                      )
                          )
                        . foldr1 (mergePeriods nrInstructions)
                        -} foldr1 lcm
                        . map (\(r,_,_,s) -> r * nrInstructions + S.findMin s)
                        . map (period network is)
                        . filter ((== 'A') . last)
                        . M.keys
                        $ network

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = stepsToxxZ <$> input
  printAnswer "Steps to reach all nodes ending in Z: " answer

main :: IO ()
main = do
  let day = "Day 08: Haunted Wasteland"
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
