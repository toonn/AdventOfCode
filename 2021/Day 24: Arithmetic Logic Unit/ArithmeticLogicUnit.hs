module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec hiding (chunk)
import Text.Megaparsec.Char

import Data.Either (either)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import qualified Data.SBV as SBV

import AoC

type Lit = Int
type Var = Char

data Instruction = Inp { var :: Var }
                 | Add { var :: Var, arg :: (Either Var Int) }
                 | Mul { var :: Var, arg :: (Either Var Int) }
                 | Div { var :: Var, arg :: (Either Var Int) }
                 | Mod { var :: Var, arg :: (Either Var Int) }
                 | Eql { var :: Var, arg :: (Either Var Int) }
                 deriving Show

type Instructions = [Instruction]
type Vars = M.Map Var [SBV.SInteger]

instruction :: Parser Instruction
instruction = do
  op <- choice
      . map (\(c,s) -> pure c <* string s)
      . zip ( [ const . Inp ]
           <> map (\c a b -> c a (fromJust b)) [Add, Mul, Div, Mod, Eql]
            )
      $ ["inp", "add", "mul", "div", "mod", "eql"]
  hspace
  target <- lowerChar
  hspace
  arg <- optional (Left <$> lowerChar <|> Right <$> signed integer)
  eol
  pure (op target arg)

parser :: Parser Instructions
parser = manyTill instruction eof

initialEnv :: Vars
initialEnv = M.fromDistinctAscList (zip "wxyz" (repeat []))

instructionConstraint :: Instruction -> Vars -> SBV.Symbolic Vars
instructionConstraint inst vars = do
  let vs = vars M.! var inst
  let v1 | null vs = SBV.literal 0
         | otherwise = head vs
  let ~v2 | Left v    <- arg inst = case vars M.! v of
                                      [] -> SBV.literal 0
                                      (v':_) -> v'
          | Right val <- arg inst = SBV.literal (fromIntegral val)
  r <- SBV.sInteger (var inst : show (length vs))
  SBV.constrain (case inst of
                   Inp _   -> SBV.inRange r (1,9)
                   Add _ _ -> v1 + v2 SBV..== r
                   Mul _ _ -> v1 * v2 SBV..== r
                   Div _ _ -> v1 `SBV.sQuot` v2 SBV..== r
                   Mod _ _ -> v1 `SBV.sRem` v2 SBV..== r
                   Eql _ _ -> SBV.oneIf (v1 SBV..== v2) SBV..== r
                )
  pure $ M.adjust (r:) (var inst) vars

programConstraints :: Instructions -> SBV.Goal
programConstraints is =
  foldr (\inst next vars -> do
          vars' <- instructionConstraint inst vars
          next vars'
        )
        (\vars -> do
          let z = head (vars M.! 'z')
          SBV.constrain (z SBV..== 0)
          let ws = vars M.! 'w'
          foldr (\w next n ->
                  next (n + 1) *> SBV.maximize ("Model digit " <> show n) w
                )
                (const (pure ()))
                ws
                0
        )
        is
        initialEnv

findLargestModelNr :: SBV.Goal -> IO Integer
findLargestModelNr goal = do
  SBV.LexicographicResult model <- SBV.optimize SBV.Lexicographic goal
  let Just modelDigits = traverse ( (\k -> SBV.getModelValue k model)
                                  . ("Model digit " <>)
                                  . show
                                  )
                                  [0..13]
  let modelNr = foldr (\w n -> 10 * n + w) 0 modelDigits
  pure modelNr

part1 :: Parsed Instructions -> IO ()
part1 input = do
  answer <- either (pure . Left)
                   ((Right <$>) . findLargestModelNr . programConstraints)
                   input
  printAnswer "Largest model number accepted by MONAD: " answer

part2 :: Parsed Instructions -> IO ()
part2 input = do
  let answer = const "P" <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 24: Arithmetic Logic Unit"
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
