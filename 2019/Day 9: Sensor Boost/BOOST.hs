#! /usr/bin/env nix-shell
#! nix-shell -i runghc
#! nix-shell -p "haskellPackages.ghcWithPackages (ps: with ps; [ vector ])"

module BOOST where

import qualified Data.Vector as V
import Data.List (intercalate, permutations)
import Data.Maybe (fromJust)
import qualified Data.Map as M

type Address = Int
type Memory = V.Vector Int

type Input = Int
type Inputs = [Input]
type Output = Int
type Outputs = [Output]

type Instruction = Int
type Opcode = Int
type Jump = Int
type RelBase = Int
type Parameter = Int
type Mode = Int
type Modes = [Mode]

type Amplifiers = [Inputs -> Maybe (Memory, Inputs, Output, Address)]

slice :: Address -> Int -> Memory -> [Parameter]
slice address n memory = V.toList v ++ replicate l 0
  where
    l = max 0 (min n (address + n - V.length memory))
    v | address < V.length memory = V.slice address (n - l) memory
      | otherwise = V.empty

(//) :: Num a => V.Vector a -> [(Int, a)] -> V.Vector a
v // [] = v
v // ias = (v V.// inBounds V.++ V.replicate (maxIndex + 1 - V.length v) 0)
  V.// outBounds
  where
    split bound = foldr (\(i,a) (mI, us, os) -> let mI' | i > mI = i
                                                        | otherwise = mI
                          in if i < bound
                            then (mI', (i,a):us, os)
                            else (mI', us, (i,a):os)
                        )
                        (-1, [],[])
    (maxIndex, inBounds, outBounds) = split (V.length v) ias

parameter :: Memory -> Parameter -> Mode -> RelBase -> Int
parameter memory param mode relBase | mode == 0 = case memory V.!? param of
                                      Nothing -> 0
                                      Just v -> v
                                    | mode == 1 = param
                                    | mode == 2 =
                                      case memory V.!? (param + relBase) of
                                        Nothing -> 0
                                        Just v -> v

write :: Memory -> Address -> Mode -> Int -> RelBase -> Memory
write memory address mode relBase value
  | mode == 0 = memory // [(address, value)]
  | mode == 2 = memory // [(address + relBase, value)]

fetchOp :: Memory -> Address -> RelBase -> Int
fetchOp memory address relBase = parameter memory address 0 relBase

add :: Memory -> [Parameter] -> Modes -> RelBase -> Memory
add memory [i,j,result] (m1:m2:m3:_) relBase =
  write memory result m3 relBase (a + b)
  where
    a = parameter memory i m1 relBase
    b = parameter memory j m2 relBase

multiply :: Memory -> [Parameter] -> Modes -> RelBase -> Memory
multiply memory [i,j,result] (m1:m2:m3:_) relBase =
  write memory result m3 relBase (a * b)
  where
    a = parameter memory i m1 relBase
    b = parameter memory j m2 relBase

inp :: Memory -> [Parameter] -> Modes -> RelBase -> Int -> Memory
inp memory [address] (mode:_) relBase i = write memory address mode relBase i

outp :: Memory -> [Parameter] -> Modes -> RelBase -> Int
outp memory [param] (mode:_) relBase = parameter memory param mode relBase

jumpTrue :: Memory -> [Parameter] -> Modes -> Address -> RelBase -> Int
jumpTrue memory [c, j] (m1:m2:_) address relBase | condition /= 0 = jump
                                                 | otherwise = 3
  where
    condition = parameter memory c m1 relBase
    jump = parameter memory j m2 relBase - address

jumpFalse :: Memory -> [Parameter] -> Modes -> Address -> RelBase -> Int
jumpFalse memory [c, j] (m1:m2:_) address relBase | condition == 0 = jump
                                                  | otherwise = 3
  where
    condition = parameter memory c m1 relBase
    jump = parameter memory j m2 relBase - address

lessThan :: Memory -> [Parameter] -> Modes -> RelBase -> Memory
lessThan memory [i,j,result] (m1:m2:m3:_) relBase
  | a < b = write memory result m3 relBase 1
  | otherwise = write memory result m3 relBase 0
  where
    a = parameter memory i m1 relBase
    b = parameter memory j m2 relBase

equals :: Memory -> [Parameter] -> Modes -> RelBase -> Memory
equals memory [i,j,result] (m1:m2:m3:_) relBase
  | a == b = write memory result m3 relBase 1
  | otherwise = write memory result m3 relBase 0
  where
    a = parameter memory i m1 relBase
    b = parameter memory j m2 relBase

adjustRelativeBase :: Memory -> [Parameter] -> Modes -> RelBase -> RelBase
adjustRelativeBase memory [a] (mode:_) relBase = relBase + b
  where
    b = parameter memory a mode relBase

decode :: Instruction -> (Opcode, Modes)
decode i = case quotRem i 100 of
  (q, r) -> (r, modes q)
  where
  modes ms = case quotRem ms 10 of
    (q, r) -> r:modes q

execOpcode :: Inputs -> Memory -> Address -> RelBase
           -> Maybe (Memory, Inputs, Maybe Output, Jump, RelBase)
execOpcode inputs memory address relBase =
  case decode (fetchOp memory address relBase) of
    (1, modes) -> Just ( add memory (parameters 3) modes relBase, inputs
                       , Nothing, 4, relBase)
    (2, modes) -> Just ( multiply memory (parameters 3) modes relBase, inputs
                       , Nothing, 4, relBase)
    (3, modes) -> Just (inp memory (parameters 1)  modes relBase (head inputs)
                       , tail inputs, Nothing , 2, relBase)
    (4, modes) -> Just (memory, inputs
                       , Just (outp memory (parameters 1) modes relBase)
                       , 2, relBase)
    (5, modes) -> Just ( memory, inputs, Nothing
                       , jumpTrue memory (parameters 2) modes address relBase
                       , relBase)
    (6, modes) -> Just (memory, inputs, Nothing
                       , jumpFalse memory (parameters 2) modes address relBase
                       , relBase)
    (7, modes) -> Just ( lessThan memory (parameters 3) modes relBase, inputs
                       , Nothing, 4, relBase)
    (8, modes) -> Just (equals memory (parameters 3) modes relBase, inputs
                       , Nothing, 4, relBase)
    (9, modes) -> Just ( memory, inputs, Nothing, 2
                       , adjustRelativeBase memory (parameters 1) modes relBase)
    (99, _) -> Nothing
    (x, _) -> error ("Unsupported opcode: " <> show x)
  where
    parameters n = slice (address + 1) n memory

step' :: Memory -> Inputs -> Address -> RelBase
      -> Maybe (Memory, Inputs, Maybe Output, Address, RelBase)
step' memory inputs address relBase =
  case execOpcode inputs memory address relBase of
    Nothing -> Nothing
    Just (next, inputs', output, jump, relBase') ->
      Just (next, inputs', output, address + jump, relBase')

step :: Memory -> Inputs -> Address -> RelBase
     -> Maybe (Memory, Inputs, Output, Address, RelBase)
step memory inputs address relBase = case step' memory inputs address relBase of
  Nothing -> Nothing
  Just (next, inputs', Nothing, address', relBase') ->
    step next inputs' address' relBase'
  Just (next, inputs', Just output, address', relBase') ->
    Just (next, inputs', output, address', relBase')

compute :: Memory -> Inputs -> Address -> RelBase
        -> Maybe (Memory, Inputs, Outputs, Address, RelBase)
compute memory inputs address relBase = foldr
  (\stp more m i a rB ->
    case stp m i a rB of
      Nothing -> Nothing
      Just (m', i', o', a', rB') -> case more m' i' a' rB' of
        Nothing -> Just (m', i', [o'], a', rB')
        Just (m'', i'', os, a'', rB'') -> Just (m'', i'', o':os, a'', rB'')
  )
  (error "Infinite list is never empty!")
  (repeat step)
  memory
  inputs
  address
  relBase

run :: Memory -> Inputs -> Outputs
run memory inputs = case compute memory inputs 0 0 of
  Nothing -> []
  Just (_, _, outputs, _, _) -> outputs

main :: IO ()
main = do
  program <- V.fromList . read . ('[':) . (++ "]") <$> readFile "input.txt"
  putStrLn . ("BOOST keycode: " <>) . init . tail . show $
    run program [1]
  putStrLn . ("Coordinates of distress signal: " <>) . init . tail . show $
    run program [2]

exampleP11 :: (Memory, Outputs)
exampleP11 = ( V.fromList
               [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
             , [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
             )

exampleP12 :: (Memory, Outputs)
exampleP12 = ( V.fromList
               [1102,34915192,34915192,7,4,7,99,0]
             , [1219070632396864]
             )

exampleP13 :: (Memory, Outputs)
exampleP13 = ( V.fromList
               [104,1125899906842624,99]
             , [1125899906842624]
             )
