#! /usr/bin/env nix-shell
#! nix-shell -i runghc
#! nix-shell -p "haskellPackages.ghcWithPackages (ps: with ps; [ vector ])"

module Computer
  ( execOpcode
  , compute
  ) where

import qualified Data.Vector as V

type Address = Int
type Memory = V.Vector Int

add :: Memory -> [Address] -> Memory
add memory [i,j,result] = memory V.// [(result, a + b)]
  where
    a = memory V.! i
    b = memory V.! j

multiply :: Memory -> [Address] -> Memory
multiply memory [i,j,result] = memory V.// [(result, a * b)]
  where
    a = memory V.! i
    b = memory V.! j

execOpcode :: Memory -> Address -> Maybe Memory
execOpcode memory address = case memory V.! address of
  1 -> Just $ add memory parameters
  2 -> Just $ multiply memory parameters
  99 -> Nothing
  where
    parameters = V.toList (V.slice (address + 1) 3 memory)

compute :: Memory -> Memory
compute memory = compute' memory 0

compute' :: Memory -> Address -> Memory
compute' memory address = case execOpcode memory address of
  Nothing -> memory
  Just next -> compute' next (address + 4)

main :: IO ()
main = (putStrLn . ("Value at position 0: " <>) . show =<< val0 12 2)
  >> (putStrLn . ("(100 * noun) + verb: " <>) . show =<< output19690720)
  where
    program = V.fromList . read . ('[':) . (++ "]") <$> readFile "input.txt"
    val0 noun verb = (V.! 0) . compute . (V.// [(1,noun), (2,verb)])
      <$> program
    nounVerb (noun, verb) = (100 * noun) + verb
    searchLin output last noun = do
      result <- val0 noun 0
      case result `compare` output of
        LT -> searchLin output result (noun + 1)
        EQ -> return (noun, 0)
        GT -> return (noun - 1, output - last)
    searchBin output noun = do
      result <- val0 noun 0
      case result `compare` output of
        LT -> searchLin output result (noun + 1)
        EQ -> return (noun, 0)
        GT -> searchBin output (noun `quot` 2)
    search output = searchBin output 99
    output19690720 = nounVerb <$> search 19690720
