module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Data.Char (isDigit, isLetter)
import Data.List (elemIndex, span)
import qualified Data.Map as M

type Rule = String
type Workflows = M.Map String [Rule]
type Part = [Int]

type Input = (Workflows, [Part])

braced :: Parser a -> Parser a
braced = between (char '{') (char '}')

commaSeparated :: Parser a -> Parser [a]
commaSeparated p = sepBy1 p (char ',')

workflow :: Parser (String, [Rule])
workflow = do
  name <- takeWhile1P (Just "Workflow name") isLetter
  rules <- braced
            (commaSeparated
              (takeWhile1P (Just "Rule character")
                           (\c -> c `elem` "<>:" || isDigit c || isLetter c)
              )
            )
  pure (name, rules)

part :: Parser Part
part = braced (commaSeparated (anySingle *> char '=' *> integer))

parser :: Parser Input
parser = (,)
       . M.fromList <$> sepEndBy1 workflow eol
      <* eol
     <*> sepEndBy1 part eol
      <* eof

apply :: String -> Part -> Maybe String
apply rule part =
  let (upToColon,rest) = span (/= ':') rule
      target | null rest = Just upToColon
             | (category:cOp:n) <- upToColon
             , Just i <- elemIndex category "xmas"
             , op <- if cOp == '<' then (<) else (>)
             , number <- (read n :: Int)
             , part !! i `op` number
             = Just (tail rest)
             | otherwise = Nothing
   in target

accepted :: Workflows -> String -> Part -> Bool
accepted workflows workflow part | rules <- workflows M.! workflow =
  foldr (\rule next ->
          let status | Just target <- apply rule part
                     = case target of
                         "A" -> True
                         "R" -> False
                         name -> accepted workflows name part
                     | otherwise = next
           in status
        )
        (error "No passing rule!")
        rules

accept :: Input -> [Part]
accept (workflows, parts) = filter (accepted workflows "in") parts

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . map sum . accept <$> input
  printAnswer "Sum of accepted part ratings: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 19: Aplenty"
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
