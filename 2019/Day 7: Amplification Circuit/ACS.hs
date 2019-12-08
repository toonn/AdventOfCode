#! /usr/bin/env nix-shell
#! nix-shell -i runghc
#! nix-shell -p "haskellPackages.ghcWithPackages (ps: with ps; [ vector ])"

module ACS where

import qualified Data.Vector as V
import Data.List (intercalate, permutations)
import Data.Maybe (fromJust, listToMaybe)
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
type Parameter = Int
type Mode = Int
type Modes = [Mode]

type Amplifiers = [Inputs -> (Memory, Outputs)]

parameter :: Memory -> Parameter -> Mode -> Int
parameter memory param mode | mode == 0 = memory V.! param
                            | mode == 1 = param

add :: Memory -> [Parameter] -> Modes -> Memory
add memory [i,j,result] (m1:m2:_) = memory V.// [(result, a + b)]
  where
    a = parameter memory i m1
    b = parameter memory j m2

multiply :: Memory -> [Parameter] -> Modes -> Memory
multiply memory [i,j,result] (m1:m2:_) = memory V.// [(result, a * b)]
  where
    a = parameter memory i m1
    b = parameter memory j m2

inp :: Memory -> [Parameter] -> Int -> Memory
inp memory [address] i = memory V.// [(address, i)]

outp :: Memory -> [Parameter] -> Modes -> Int
outp memory [param] (mode:_) = parameter memory param mode

jumpTrue :: Memory -> [Parameter] -> Modes -> Address -> Int
jumpTrue memory [c, j] (m1:m2:_) address | condition /= 0 = jump
                                         | otherwise = 3
  where
    condition = parameter memory c m1
    jump = parameter memory j m2 - address

jumpFalse :: Memory -> [Parameter] -> Modes -> Address -> Int
jumpFalse memory [c, j] (m1:m2:_) address | condition == 0 = jump
                                          | otherwise = 3
  where
    condition = parameter memory c m1
    jump = parameter memory j m2 - address

lessThan :: Memory -> [Parameter] -> Modes -> Memory
lessThan memory [i,j,result] (m1:m2:_) | a < b = memory V.// [(result, 1)]
                                       | otherwise = memory V.// [(result, 0)]
  where
    a = parameter memory i m1
    b = parameter memory j m2

equals :: Memory -> [Parameter] -> Modes -> Memory
equals memory [i,j,result] (m1:m2:_) | a == b = memory V.// [(result, 1)]
                                     | otherwise = memory V.// [(result, 0)]
  where
    a = parameter memory i m1
    b = parameter memory j m2

decode :: Instruction -> (Opcode, Modes)
decode i = case quotRem i 100 of
  (q, r) -> (r, modes q)
  where
  modes ms = case quotRem ms 10 of
    (q, r) -> r:modes q

execOpcode :: Inputs -> Memory -> Address
           -> Maybe (Memory, Inputs, Maybe Output, Jump)
execOpcode inputs memory address = case decode (memory V.! address) of
  (1, modes) -> Just (add memory (parameters 3) modes, inputs, Nothing, 4)
  (2, modes) -> Just (multiply memory (parameters 3) modes, inputs, Nothing, 4)
  (3, _) ->
    Just (inp memory (parameters 1) (head inputs), tail inputs, Nothing, 2)
  (4, modes) ->
    Just (memory, inputs, Just (outp memory (parameters 1) modes), 2)
  (5, modes) -> Just (memory, inputs, Nothing
                     , jumpTrue memory (parameters 2) modes address)
  (6, modes) -> Just (memory, inputs, Nothing
                     , jumpFalse memory (parameters 2) modes address)
  (7, modes) -> Just (lessThan memory (parameters 3) modes, inputs, Nothing, 4)
  (8, modes) -> Just (equals memory (parameters 3) modes, inputs, Nothing, 4)
  (99, _) -> Nothing
  where
    parameters n = V.toList (V.slice (address + 1) n memory)

compute' :: Inputs -> Memory -> Address -> (Memory, Outputs)
compute' inputs memory address = case execOpcode inputs memory address of
  Nothing -> (memory, [])
  Just (next, inputs', output, jump) ->
    let (m, os) = compute' inputs' next (address + jump)
    in case output of
      Nothing -> (m, os)
      Just o -> (m, o:os)

compute :: Inputs -> Memory -> (Memory, Outputs)
compute inputs memory = compute' inputs memory 0

amplifier :: Memory -> Input -> [Input] -> (Memory, Outputs)
amplifier memory phaseSetting inputs = compute (phaseSetting:inputs) memory

amplifiers' :: Inputs -> Amplifiers -> ([Memory], Outputs)
amplifiers' inputs amps =
  foldr (\amp more i -> case amp i of
          (m, o) -> case more o of
            (mems, o') -> (m:mems, o')
        )
        (\i -> ([], i))
        amps
        inputs

amplifiers :: [Memory] -> Inputs -> Inputs -> ([Memory], Outputs)
amplifiers memories inputs phaseSettings = amplifiers' inputs
  (map (\(m, pS) -> amplifier m pS) (zip memories phaseSettings))

maximumThrusterSignal :: Memory -> Output
maximumThrusterSignal memory =
  maximum (map (head . snd . amplifiers (replicate 5 memory) [0])
               (permutations [0..4]))

feedback :: ([Memory], Inputs) -> Output
feedback (amps, inputs) =
  case amplifiers' inputs (toAmps amps) of
    (_, []) -> head inputs
    o -> feedback o
  where
    toAmps = map (\m is -> compute is m)

maximumFeedbackSignal :: Memory -> Output
maximumFeedbackSignal memory =
  feedback . amplifiers memories [0] . snd .  M.findMax $
  foldr (\phaseSettings m ->
          M.insert (head . snd $ amplifiers memories [0] phaseSettings)
                   phaseSettings
                   m
        )
        M.empty
        (permutations [5..9])
  where
    memories = replicate 5 memory

mFS memory = M.findMax $
  foldr (\phaseSettings m ->
          M.insert (head . snd $ amplifiers memories [0] phaseSettings)
                   phaseSettings
                   m
        )
        M.empty
        (permutations [5..9])
  where
    memories = replicate 5 memory

main :: IO ()
main = do
  program <- V.fromList . read . ('[':) . (++ "]") <$> readFile "input.txt"
  let mTS = maximumThrusterSignal program
  putStrLn ("Maximum thruster signal: " <> show mTS)
  let mFTS = maximumFeedbackSignal program
  putStrLn ("Maximum feedback thruster signal: " <> show mFTS)

exampleP11 :: (Memory, Inputs, Output)
exampleP11 = ( V.fromList [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
             , [4,3,2,1,0]
             , 43210
             )

exampleP12 :: (Memory, Inputs, Output)
exampleP12 = ( V.fromList [ 3,23,3,24,1002,24,10,24,1002,23,-1,23
                          , 101,5,23,23,1,24,23,23,4,23,99,0,0
                          ]
             , [0,1,2,3,4]
             , 54321
             )

exampleP13 :: (Memory, Inputs, Output)
exampleP13 = ( V.fromList [ 3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33
                          , 1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0
                          ]
             , [1,0,4,3,2]
             , 65210
             )

exampleP21 :: (Memory, Inputs, Output)
exampleP21 = ( V.fromList [ 3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26
                          , 27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5
                          ]
             , [9,8,7,6,5]
             , 139629729
             )

exampleP22 :: (Memory, Inputs, Output)
exampleP22 =
  ( V.fromList
    [ 3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54
    , -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4
    , 53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10
    ]
  , [9,7,8,5,6]
  , 18216
  )
