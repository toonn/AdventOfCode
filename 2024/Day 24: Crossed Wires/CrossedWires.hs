module Main where

import Criterion.Main
import Criterion.Types
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Control.Arrow ((***))
import Data.Bits ( (.&.)
                 , (.^.)
                 , (.|.)
                 , (.<<.)
                 )
import Data.Char (isAlphaNum)
import Data.List (intercalate, sort)
import Data.Tuple (swap)
import qualified Data.Map as M
import qualified Data.Set as S

newtype Swaps = Swaps [String]

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

arrange :: Input -> (M.Map String Int, M.Map String (String, String, String))
arrange = M.fromList *** M.fromList . map swap

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

decimal :: (M.Map String Int, M.Map String (String, String, String)) -> Int
decimal (initM, gateM) = let zs = filter (\(l:_) -> l == 'z')
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
  let answer = decimal . arrange <$> input
  printAnswer "Decimal on z wires: " answer

swaps :: M.Map String (String, String, String) -> Swaps
swaps gateM
  = Swaps
  . foldr (\(k, (l,op,r)) ->
            let ins | ('z':nr) <- k, (nr /= "45" || op /= "OR") && op /= "XOR"
                    = (k:)
                    | Just (ll,lop,_) <- gateM M.!? l
                    , Just (rl,rop,_) <- gateM M.!? r
                    = let inp | op == "OR" && [lop,rop] /= ["AND","AND"]
                              = if lop /= "AND"
                                then (l:) else (r:)
                              | op == "XOR" && (drop 1 k == "01")
                             && sort [lop,rop] /= ["AND","XOR"]
                              = if lop == "XOR" && (drop 1 ll /= "01")
                                then (l:) else (r:)
                              | op == "XOR" && not (drop 1 k == "01")
                             && sort [lop,rop] /= ["OR","XOR"]
                              = if lop == "XOR" && (drop 1 k /= drop 1 ll)
                                || lop == "AND"
                                then (l:) else (r:)
                              | otherwise = id
                       in inp
                    | otherwise = id
             in ins
          )
          mempty
  . M.assocs
  $ gateM

instance Show Swaps where
  show (Swaps swaps) = intercalate "," . sort $ swaps

-- dkj,hkj,hnv,kfm,std,vmv,z21,z35 not right because it's the gates that have
--   erroneous *inputs*
-- bqn,cvq,hnv,hth,jnb,tqr,vmv,vwh not the right inputs?
-- hnv,hth,tqr,vmv,z07,z20,z28,z45 not right z45 is the last *carry*!
part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = swaps . snd . arrange <$> input
  printAnswer "Wires to swap: " answer

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
