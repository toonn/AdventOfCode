module Main where

import Criterion.Main
import Data.List (isPrefixOf)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Void (Void)
import System.FilePath ((</>))
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Paths_AoC2020

type Parser = Parsec Void String
type Parsed a = Either (ParseErrorBundle String Void) a

type Ticket = [Integer]
type Tickets = ( M.Map String (S.Set Integer)
               , Ticket
               , [Ticket]
               )

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space hspace1 empty empty)

integer :: Parser Integer
integer = lexeme L.decimal

range :: Parser (S.Set Integer)
range = do
  start <- integer
  char '-'
  end <- integer
  pure (S.fromList [start..end])

rule :: Parser (String, S.Set Integer)
rule = do
  field <- takeWhile1P (Just "Field character") (`elem` (' ':['a'..'z']))
  lexeme (char ':')
  ranges <- sepBy range (lexeme (string "or"))
  pure (field, mconcat ranges)

ticket :: Parser Ticket
ticket = sepBy1 integer (char ',')

tickets :: Parser Tickets
tickets = do
  rules <- sepEndBy rule eol
  eol
  lexeme (string "your ticket:")
  eol
  myTicket <- ticket
  eol
  eol
  lexeme (string "nearby tickets:")
  eol
  tickets <- sepEndBy ticket eol
  eof
  pure (M.fromList rules, myTicket, tickets)

readInput :: String -> IO (Parsed Tickets)
readInput day = do
  inputFile <- getDataFileName (day </> "input.txt")
  parse tickets inputFile
    <$> readFile inputFile

printAnswer :: Show a => String -> Parsed a -> IO ()
printAnswer question answer =
  either (putStrLn . errorBundlePretty)
         (putStrLn . (question <>) . show)
         answer

scanningErrorRate :: Tickets -> Integer
scanningErrorRate (rules, _, tickets) =
  foldr (\tNrs eR -> eR + S.foldr (+) 0 (tNrs S.\\ valid)) 0 ticketNrs
  where
    ticketNrs = map S.fromList tickets
    valid = M.foldr (<>) S.empty rules

part1 :: Parsed Tickets -> IO ()
part1 input = do
  let answer = scanningErrorRate <$> input
  printAnswer "Nearby ticket scanning error rate: " answer

departureFieldProduct :: Tickets -> Integer
departureFieldProduct (rules, myTicket, tickets) =
  foldr (*) 1 departureFields
  where
    valid = M.foldr (<>) S.empty rules
    validTickets = foldr (\t vTs -> if S.size (S.fromList t S.\\ valid) == 0
                                    then t:vTs else vTs)
                         []
                         tickets
    fieldSets = foldr (\t next fSets -> next (map (\(v, vs) -> S.insert v vs)
                                                  (zip t fSets)
                                             )
                      )
                      id
                      validTickets
                      (replicate (length myTicket) S.empty)
    possibleFields nrs = M.foldrWithKey (\k v ks -> if S.size (nrs S.\\ v) == 0
                                                    then S.insert k ks
                                                    else ks)
                                        S.empty
                                        rules
    possibleFieldSets = map possibleFields fieldSets
    eliminate fieldSets | fieldSets /= fieldSets' = eliminate fieldSets'
                        | otherwise = fieldSets'
      where
        unique = foldr (\s uniq -> if S.size s == 1 then uniq <> s else uniq)
                       S.empty
                       fieldSets
        fieldSets' = map (\s -> if S.size s == 1 then s else s S.\\ unique)
                         fieldSets
    fieldOrder = map (S.foldr const "") (eliminate possibleFieldSets)
    departureFields = map snd (filter (\(f,_) -> "departure" `isPrefixOf` f)
                                      (zip fieldOrder myTicket))

part2 :: Parsed Tickets -> IO ()
part2 input = do
  let answer = departureFieldProduct <$> input
  printAnswer "Product of all six departure fields: " answer

main :: IO ()
main = do
  let day = "Day 16: Ticket Translation"
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
