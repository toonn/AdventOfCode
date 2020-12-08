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

data Instruction = Acc Integer | Jmp Integer | Nop
type Program = V.Vector Instruction
data ProgramState = ProgramState { address :: Integer
                                 , argument :: Integer
                                 }

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
    "nop" -> Nop

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
    Nothing -> error (L.intercalate " "
      [ "Address"
      , show addr
      , "exceeds the upper bound of the program"
      , show (V.length program)
      ])
    Just inst -> case inst of
      Acc inc -> ProgramState (addr + 1) (acc + inc)
      Jmp offset -> ProgramState (addr + offset) acc
      Nop -> ProgramState (addr + 1) acc

accumulator :: ProgramState -> Program -> Integer
accumulator pState program = go (step pState program)
                                (S.singleton (address pState))
  where
    go pS@(ProgramState addr acc) visited
      | addr `S.member` visited = acc
      | otherwise = go (step pS program) (S.insert addr visited)

part1 :: Parsed Program -> IO ()
part1 input = do
  let answer = accumulator (ProgramState 0 0) <$> input
  printAnswer "Accumulator after one iteration: " answer

part2 :: Parsed Program -> IO ()
part2 input = do
  let answer = const "P" <$> input
  printAnswer "Not an answer: " answer

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
