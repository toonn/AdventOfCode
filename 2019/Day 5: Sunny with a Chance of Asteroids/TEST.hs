#! /usr/bin/env nix-shell
#! nix-shell -i runghc
#! nix-shell -p "haskellPackages.ghcWithPackages (ps: with ps; [ vector ])"

module Computer where

import qualified Data.Vector as V
import Data.List (intercalate)

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

jumpTrue :: Memory -> [Parameter] -> Modes -> Int
jumpTrue memory [c, j] (m1:m2:_) | condition /= 0 = jump
                                 | otherwise = 3
  where
    condition = parameter memory c m1
    jump = parameter memory j m2

jumpFalse :: Memory -> [Parameter] -> Modes -> Int
jumpFalse memory [c, j] (m1:m2:_) | condition == 0 = jump
                                  | otherwise = 3
  where
    condition = parameter memory c m1
    jump = parameter memory j m2

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
  (5, modes) ->
    Just (memory, inputs, Nothing, jumpTrue memory (parameters 2) modes)
  (6, modes) ->
    Just (memory, inputs, Nothing, jumpFalse memory (parameters 2) modes)
  (7, modes) -> Just (lessThan memory (parameters 3) modes, inputs, Nothing, 4)
  (8, modes) -> Just (equals memory (parameters 3) modes, inputs, Nothing, 4)
  (99, _) -> Nothing
  where
    parameters n = V.toList (V.slice (address + 1) n memory)

compute :: Inputs -> Memory -> (Memory, Outputs)
compute inputs memory = compute' inputs memory 0

compute' :: Inputs -> Memory -> Address -> (Memory, Outputs)
compute' inputs memory address = case execOpcode inputs memory address of
  Nothing -> (memory, [])
  Just (next, inputs', output, jump) ->
    let (m, os) = compute' inputs' next (address + jump)
    in case output of
      Nothing -> (m, os)
      Just o -> (m, o:os)

diagnostic :: Outputs -> String
diagnostic output = either id id $
  foldr (\o more ix ->
          let i = maybe 0 fst ix
              moar = more (Just (i + 1, o))
              diag | o == 0 = either Left Left moar
                   | otherwise = case moar of
                     Left _ -> Left (intercalate " "
                       [ "Failed diagnostic"
                       , show o
                       , "at"
                       , show i
                       ])
                     Right msg -> Left msg
          in diag
        )
        (\ix -> case ix of
          Nothing -> Right "No output"
          Just (_, c) -> Right (show c)
        )
        output
        Nothing

main :: IO ()
main = do
  program <- V.fromList . read . ('[':) . (++ "]") <$> readFile "input.txt"
  let (_, o1) = compute [1] program
  putStrLn ("Diagnostic for input 1: " <> diagnostic o1)
  let (_, o2) = compute [5] program
  putStrLn ("Diagnostic code for system ID 5: " <> diagnostic o2)

jumpTestPosMode :: (Memory, [(Inputs, Output)])
jumpTestPosMode = ( V.fromList [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]
                  , [([0], 0)
                    , ([1..20], 1)
                    ]
                  )

jumpTestImmMode :: (Memory, [(Inputs, Output)])
jumpTestImmMode = ( V.fromList [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]
                  , [([0], 0)
                    , ([1..20], 1)
                    ]
                  )

ltEqGtTest :: (Memory, [(Inputs, Output)])
ltEqGtTest = ( V.fromList [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                           1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                           999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
             , [([0..8-1], 999)
               , ([8], 1000)
               , ([8+1..20], 1001)
               ]
             )
