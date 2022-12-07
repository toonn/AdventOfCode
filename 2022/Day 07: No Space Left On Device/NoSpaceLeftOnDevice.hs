module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Char (isLetter)

import AoC

type Name = String
type Size = Int
data FSObj = Dir Name [FSObj] | File Name Size deriving Show
data Command = CD Name | LS [FSObj] deriving Show

type Input = [Command]

name :: Parser Name
name = takeWhileP (Just "File system name") (\c -> isLetter c || c `elem` "./")

fsobjStub :: Parser FSObj
fsobjStub = do
  (\n -> Dir n []) <$> (string "dir" *> hspace *> name <* eol)
  <|> (do s <- integer
          hspace
          n <- name
          eol
          pure (File n s)
      )

command :: Parser Command
command = do
  char '$'
  hspace
  (CD <$> (string "cd" *> hspace *> name <* eol)
   <|> LS <$> ( string "ls" *> eol
             *> manyTill fsobjStub
                         (try (lookAhead (char '$' *> pure () <|> eof)))
              ))


parser :: Parser Input
parser = manyTill command eof

insert :: [Name] -> [FSObj] -> FSObj -> FSObj
insert [] contents (Dir n' _) = Dir n' contents
insert (n:ns) contents hierarchy
  = case hierarchy of
      Dir n' contents'
        | n == n' -> if ns == []
                     then Dir n' contents
                     else Dir n' (map (insert ns contents) contents')
      _ -> hierarchy

hierarchyFromCommands :: [Command] -> FSObj
hierarchyFromCommands cmds
  = foldr (\cmd more pathStack hierarchy ->
            case cmd of
              CD ".." -> more (tail pathStack) hierarchy
              CD n    -> more (n : pathStack) hierarchy
              LS contents -> more pathStack
                                  (insert (reverse pathStack)
                                          contents
                                          hierarchy
                                  )
          )
          (const id)
          cmds
          []
          (Dir "/" [])

dirSizes :: FSObj -> [Size]
dirSizes (Dir _ contents)
  = foldr (\fsobj (c:sizes) -> case fsobj of
            File _ s -> (c + s : sizes)
            d@(Dir _ _) -> case dirSizes d of
             dSizes -> (c + head dSizes : (dSizes <> sizes))
          )
          [0]
          contents

totalDirectorySizes :: Input -> [Int]
totalDirectorySizes = dirSizes . hierarchyFromCommands

sumTotalDirectoriesUnder100000 :: [Int] -> Int
sumTotalDirectoriesUnder100000 = sum . filter (< 100000)

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sumTotalDirectoriesUnder100000 . totalDirectorySizes <$> input
  printAnswer "Sum of total sizes of directories under 100000: " answer

sizeOfSmallestFreeingEnoughSpace :: [Int] -> Int
sizeOfSmallestFreeingEnoughSpace sizes = minimum . filter (> targetFree) $ sizes
  where
    currentFree = 70000000 - (head sizes)
    targetFree = 30000000 - currentFree

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = sizeOfSmallestFreeingEnoughSpace . totalDirectorySizes <$> input
  printAnswer "Size of smallest directory that frees up enough space: " answer

main :: IO ()
main = do
  let day = "Day 07: No Space Left On Device"
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
