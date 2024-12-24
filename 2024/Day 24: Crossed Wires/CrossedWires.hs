module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Data.Bits ( (.&.)
                 , (.^.)
                 , (.|.)
                 , (.<<.)
                 )
import Data.Char (isAlphaNum)
import Data.Tuple (swap)
import qualified Data.Map as M

type Input = ([(String, Int)], [((String, String, String), String)])

wire :: Parser String
wire = takeWhile1P (Just "A wire") isAlphaNum

initialValues :: Parser [(String, Int)]
initialValues = sepEndBy1 ((,) <$> wire <* lexeme (char ':') <*> integer) eol

gates :: Parser [((String, String, String), String)]
gates = sepEndBy1 ( (,)
                <$> ( (,,)
                  <$> lexeme wire
                  <*> lexeme (choice . map string $ ["AND", "OR", "XOR"])
                  <*> lexeme wire
                    )
                 <* lexeme (string "->")
                <*> wire
                  )
                  eol

parser :: Parser Input
parser = (,) <$> initialValues <* eol <*> gates <* eof

draw :: M.Map String (String, String, String) -> M.Map String Int -> String
     -> M.Map String Int
draw gateM stateM wire
  | M.member wire stateM = stateM
  | otherwise = let (l,op,r) = gateM M.! wire
                    stateM' = draw gateM (draw gateM stateM l) r
                    lv = stateM' M.! l
                    rv = stateM' M.! r
                    opf = case op of
                            "AND" -> (.&.)
                            "OR"  -> (.|.)
                            "XOR" -> (.^.)
                 in M.insert wire (lv `opf` rv) stateM'

decimal :: Input -> Int
decimal (initials, gates) = let initM = M.fromList initials
                                gateM = M.fromList . map swap $ gates
                                zs = filter (\(l:_) -> l == 'z')
                                   . map fst
                                   . M.toDescList
                                   $ gateM
                             in foldr (\wire more d stateM ->
                                        let stateM' = draw gateM stateM wire
                                            d' = d .<<. 1 + stateM' M.! wire
                                         in more d' stateM'
                                      )
                                      const
                                      zs
                                      0
                                      initM

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = decimal <$> input
  printAnswer "Decimal on z wires: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const 'P' <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 24: Crossed Wires"
  input <- readInput day parser
  putStrLn ""
  part1 input
  part2 input
  putStrLn ""
  defaultMainWith (defaultConfig { resamples = 1 }) [
      bgroup "AoC"
        [ bench "Part 1" $ nfIO (silence $ readInput day parser >>= part1)
        , bench "Part 2" $ nfIO (silence $ readInput day parser >>= part2)
        ]
    ]
