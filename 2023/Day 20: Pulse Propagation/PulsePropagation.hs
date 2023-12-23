module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Data.Char (isLetter)
import qualified Data.Map as M
import qualified Data.Set as S

type Name = String
type Type = Char
type Destinations = [Name]
type Configuration = M.Map Name (Type, Destinations)
type Pulse = Bool
type States = (Int, Int, M.Map Name Bool, M.Map Name (M.Map Name Pulse))
type Pulses = [(Name, Pulse, Name)]

type Input = [(Name, (Type, Destinations))]

name :: Parser Name
name = takeWhile1P (Just "Module name character") isLetter

moduleDescription :: Parser (Name, (Type, Destinations))
moduleDescription = do
  ty <- (char '%')
    <|> (char '&')
    <|> (pure '=')
  nm <- lexeme name
  lexeme (string "->")
  ds <- sepBy1 name (lexeme (char ','))
  pure (nm, (ty, ds))

parser :: Parser Input
parser = sepEndBy1 moduleDescription eol <* eof

inputs :: Configuration -> M.Map Name [Name]
inputs = M.foldrWithKey (\n (_,ds) is ->
                          foldr (\d -> M.insertWith (<>) d [n])
                                is
                                ds
                        )
                        M.empty

initialStates :: Configuration -> States
initialStates c = ( 0
                  , 0
                  , M.map (const False) . M.filter ((== '%') . fst) $ c
                  , M.mapWithKey (\n _ -> M.fromList
                                            . map (\i -> (i, False))
                                            $ inputs c M.! n
                                     )
                      . M.filter ((== '&') . fst)
                      $ c
                  )

flow :: Configuration -> States -> Pulses -> States
flow _ states [] = states
flow configuration
     (lPs, hPs, flipflops, conjunctions)
     ((from, pulse, to):pulses)
  = let (lPs', hPs') | pulse = (lPs, hPs + 1)
                     | otherwise = (lPs + 1, hPs)
        (ty, destinations) = M.findWithDefault ('=', []) to configuration
        flipflops' | pulse = flipflops
                   | ty == '%' = M.adjust not to flipflops
                   | otherwise = flipflops
        conjunctions' | ty == '&' = M.adjust (M.insert from pulse)
                                             to
                                             conjunctions
                      | otherwise = conjunctions
        on = flipflops M.! to
        pulse' | ty == '%', not pulse = not on
               | ty == '&' = (not . and) . M.elems $ (conjunctions' M.! to)
               | ty == '=' = pulse
        pulses' | ty == '%', pulse = []
                | otherwise = map (\d -> (to, pulse', d)) destinations
     in flow configuration
             (lPs', hPs', flipflops', conjunctions')
             (pulses <> pulses')

push :: Configuration -> States -> Int -> States
push _ states 0 = states
push configuration states n =
  let states' = flow configuration states [("button", False, "broadcaster")]
   in push configuration states' (n - 1)

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = (\(lPs,hPs,_,_) -> lPs * hPs)
             . (\configuration -> push configuration
                                       (initialStates configuration)
                                       1000
               )
             . M.fromList
           <$> input
  printAnswer "Product of low and high pulses sent: " answer

-- Entirely tailored solution to my input.
-- Find the period of all the inputs to the conjunction "zh" right before "rx".
-- Then guess that they output a high pulse on the last step of the period.
-- Find the LCM of the periods because that's when all of them will
-- synchronise.
period :: Configuration -> Int
period configuration =
  let states = initialStates configuration
      is = M.map S.fromList . inputs $ configuration
      dependencies = transitiveClosure is
      go :: States -> Int -> Name -> Int
      go ss n m | n > 0, is <- dependencies M.! m
                , (_,_,sFFs,sCs) <- ss
                , (_,_,statesFFs,statesCs) <- states
                , M.restrictKeys sFFs is == M.restrictKeys statesFFs is
                , M.restrictKeys sCs is == M.restrictKeys statesCs is
                = n
                | otherwise = go (push configuration ss 1) (n + 1) m

      states' = flow configuration states [("button", False, "broadcaster")]
   in foldr1 lcm . map (go states 0) . S.toList $ is M.! "zh"


part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = period . M.fromList <$> input
  printAnswer "Button presses for a low pulse to rx: " answer

main :: IO ()
main = do
  let day = "Day 20: Pulse Propagation"
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
