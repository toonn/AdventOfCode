module Main where

import Criterion.Main
import Data.Char (isLower)
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Void (Void)
import System.FilePath ((</>))
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Paths_AoC2020

type Parser = Parsec Void String
type Parsed a = Either (ParseErrorBundle String Void) a

data Instruction = Acc Integer | Jmp Integer | Nop Integer
type Program = V.Vector Instruction
data ProgramState = ProgramState { address :: Integer
                                 , accumulator :: Integer
                                 }
                  | FinalState { accumulator :: Integer }

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space hspace1 empty empty)

integer :: Parser Integer
integer = lexeme L.decimal

signedInteger :: Parser Integer
signedInteger = L.signed (L.space empty empty empty) integer

anyWord :: Parser String
anyWord = lexeme (takeWhile1P (Just "Lowercase") isLower)

instruction :: Parser Instruction
instruction = do
  inst <- anyWord
  arg <- signedInteger
  pure $ case inst of
    "acc" -> Acc arg
    "jmp" -> Jmp arg
    "nop" -> Nop arg

program :: Parser Program
program = V.fromList <$> sepEndBy instruction eol

readInput :: String -> IO (Parsed Program)
readInput day = do
  inputFile <- getDataFileName (day </> "input.txt")
  parse program inputFile
    <$> readFile inputFile

printAnswer :: Show a => String -> Parsed a -> IO ()
printAnswer question answer =
  either (putStrLn . errorBundlePretty)
         (putStrLn . (question <>) . show)
         answer

step :: ProgramState -> Program -> ProgramState
step (ProgramState addr acc) program =
  case program V.!? fromInteger addr of
    Nothing | V.length program == fromInteger addr -> FinalState acc
            | otherwise -> error (L.intercalate " "
              [ "Address"
              , show addr
              , "exceeds the upper bound of the program"
              , show (V.length program)
              ])
    Just inst -> case inst of
      Acc inc -> ProgramState (addr + 1) (acc + inc)
      Jmp offset -> ProgramState (addr + offset) acc
      Nop _ -> ProgramState (addr + 1) acc

accumulatedAfterIteration :: ProgramState -> Program -> Integer
accumulatedAfterIteration pState program = go (step pState program)
                                              (S.singleton (address pState))
  where
    go pS@(ProgramState addr acc) visited
      | addr `S.member` visited = acc
      | otherwise = go (step pS program) (S.insert addr visited)

part1 :: Parsed Program -> IO ()
part1 input = do
  let answer = accumulatedAfterIteration (ProgramState 0 0) <$> input
  printAnswer "Accumulator after one iteration: " answer

alterations :: Program -> [Program]
alterations program = V.ifoldr alter [] program
  where
    alter index (Jmp v) ps = program V.// [(index, Nop v)]:ps
    alter index (Nop v) ps | v /= 0 = program V.// [(index, Jmp v)]:ps
    alter _ _ ps = ps

accumulatedAfterTermination :: ProgramState -> Program -> Integer
accumulatedAfterTermination pState program =
  foldr (go pState (S.empty))
        (error "No terminating alteration found")
        (alterations program)
  where
    go pS@(ProgramState addr acc) visited prog next
      | addr `S.member` visited = next
      | otherwise = go (step pS prog) (S.insert addr visited) prog next
    go (FinalState acc) _ _ _ = acc

part2 :: Parsed Program -> IO ()
part2 input = do
  let answer = accumulatedAfterTermination (ProgramState 0 0) <$> input
  printAnswer "Accumulator after termination: " answer

main :: IO ()
main = do
  let day =  "Day 8: Handheld Halting"
  input <- readInput day
  putStrLn ""
  part1 input
  part2 input
  putStrLn ""
  defaultMain [
      bgroup "AoC"
        [ bench "Part 1" $ nfIO (silence $ readInput day >>= part1)
        , bench "Part 2" $ nfIO (silence $ readInput day >>= part2)
        ]
    ]
