#! /usr/bin/env nix-shell
#! nix-shell -i runghc
#! nix-shell -p "haskellPackages.ghcWithPackages (ps: with ps; [ containers ])"

module NanoFactory where

import Data.List (intercalate)
import qualified Data.Map as M
import Text.ParserCombinators.ReadP

type Ingredient = String
type Quantity = Int
type Inputs = [(Ingredient, Quantity)]
type Reactions = M.Map Ingredient (Quantity, Inputs)
type Remainders = M.Map Ingredient Quantity

applySnd :: (b -> c) -> (a,b) -> (a,c)
applySnd = fmap

ore :: Reactions -> Remainders -> Int -> Inputs -> Int
ore _ _ nrORE [] = nrORE
ore reactions remainders nrORE (("ORE", needed):inputs) =
  ore reactions remainders (nrORE + needed) inputs
ore reactions remainders nrORE ((ingredient, needed):inputs) =
  case reactions M.! ingredient of
    (provided, inputs') -> case remainders M.!? ingredient of
      Nothing -> case needed `quotRem` provided of
        (q,r) | r == 0 -> ore reactions
                              remainders
                              nrORE
                              (inputs <> map (applySnd (*q)) inputs')
              | otherwise -> ore reactions
                                 (M.insert ingredient (provided - r) remainders)
                                 nrORE
                                 (inputs <> map (applySnd (*(q + 1))) inputs')
      Just remaining
        | remaining == needed -> ore reactions
                                     (M.delete ingredient remainders)
                                     nrORE
                                     inputs
        | remaining > needed -> ore reactions
                                    (M.insert ingredient
                                              (remaining - needed)
                                              remainders
                                    )
                                    nrORE
                                    inputs
        | otherwise -> case (needed - remaining) `quotRem` provided of
          (q,r) | r == 0 -> ore reactions
                                (M.delete ingredient remainders)
                                nrORE
                                (inputs <> map (applySnd (*q)) inputs')
                | otherwise -> ore reactions
                                   (M.insert ingredient (provided-r) remainders)
                                   nrORE
                                   (inputs <> map (applySnd (*(q + 1))) inputs')

totalORE :: Reactions -> Int
totalORE reactions = ore reactions M.empty 0 . snd $ reactions M.! "FUEL"

totalOREForFUEL :: Reactions -> Int -> Int
totalOREForFUEL reactions nr = case reactions M.! "FUEL" of
  (quantity, inputs) -> case nr `quotRem` quantity of
    (q,r) | r == 0 ->
      totalORE (M.insert "FUEL" (q, map (applySnd (*q)) inputs) reactions)
          | otherwise ->
      totalORE (M.insert "FUEL" (q, map (applySnd (*(q + 1))) inputs) reactions)

search :: Reactions -> Int -> Int -> Int
search reactions optimum nr = case totalOREForFUEL reactions nr of
  x | x < 1000000000000 -> search reactions nr (nr + optimum)
    | x > 1000000000000 -> case (nr - optimum) `quotRem` 2 of
      (q,r) | q == 0 -> optimum
            | otherwise -> search reactions optimum (q + optimum)
    | otherwise -> nr

maximumFUEL :: Reactions -> Int
maximumFUEL reactions = search reactions estimate estimate
  where estimate = (1000000000000 `quot` (totalORE reactions))

parseIngredient :: ReadP (Ingredient, Quantity)
parseIngredient = do
  quantity <- munch (`elem` ['0'..'9'])
  skipSpaces
  ingredient <- munch (`elem` ['A'..'Z'])
  return (ingredient, read quantity)

parseReactions :: String -> Reactions
parseReactions = foldr (\((oI,oQ), is) -> M.insert oI (oQ, is)) M.empty
  . fst . last . readP_to_S (many $ do
  skipSpaces
  inputs <- sepBy parseIngredient (char ',' >> skipSpaces)
  skipSpaces >> string "=>" >> skipSpaces
  output <- parseIngredient
  return (output, inputs)
  )

main :: IO ()
main = do
  reactions <- parseReactions <$> readFile "input.txt"
  putStrLn . ("Minimum required amount of ORE: " <>) . show $
    totalORE reactions
  putStrLn . ("Maximum amount of FUEL: " <>) . show $
    maximumFUEL reactions

exampleP11 :: (String, Int)
exampleP11 = ( intercalate "\n" [ "10 ORE => 10 A"
                                , "1 ORE => 1 B"
                                , "7 A, 1 B => 1 C"
                                , "7 A, 1 C => 1 D"
                                , "7 A, 1 D => 1 E"
                                , "7 A, 1 E => 1 FUEL"
                                ]
             , 31
             )

exampleP12 :: (String, Int)
exampleP12 = ( intercalate "\n" [ "9 ORE => 2 A"
                                , "8 ORE => 3 B"
                                , "7 ORE => 5 C"
                                , "3 A, 4 B => 1 AB"
                                , "5 B, 7 C => 1 BC"
                                , "4 C, 1 A => 1 CA"
                                , "2 AB, 3 BC, 4 CA => 1 FUEL"
                                ]
             , 165
             )

exampleP13 :: (String, Int, Int)
exampleP13 = ( intercalate "\n" [ "157 ORE => 5 NZVS"
                                , "165 ORE => 6 DCFZ"
                                , "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, \
                                  \9 GPVTF, 48 HKGWZ => 1 FUEL"
                                , "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ"
                                , "179 ORE => 7 PSHF"
                                , "177 ORE => 5 HKGWZ"
                                , "7 DCFZ, 7 PSHF => 2 XJWVT"
                                , "165 ORE => 2 GPVTF"
                                , "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"
                                ]
             , 13312
             , 82892753
             )

exampleP14 :: (String, Int, Int)
exampleP14 = ( intercalate "\n" [ "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => \
                                  \1 STKFG"
                                , "17 NVRVD, 3 JNWZP => 8 VPVL"
                                , "53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, \
                                  \68 CXFTF, 25 GNMV => 1 FUEL"
                                , "22 VJHF, 37 MNCFX => 5 FWMGM"
                                , "139 ORE => 4 NVRVD"
                                , "144 ORE => 7 JNWZP"
                                , "5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF \
                                  \=> 3 HVMC"
                                , "5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV"
                                , "145 ORE => 6 MNCFX"
                                , "1 NVRVD => 8 CXFTF"
                                , "1 VJHF, 6 MNCFX => 4 RFSQX"
                                , "176 ORE => 6 VJHF"
                                ]
             , 180697
             , 5586022
             )

exampleP15 :: (String, Int, Int)
exampleP15 = ( intercalate "\n" [ "171 ORE => 8 CNZTR"
                                , "7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, \
                                  \2 MZWV, 1 RJRHP => 4 PLWSL"
                                , "114 ORE => 4 BHXH"
                                , "14 VRPVC => 6 BMBT"
                                , "6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, \
                                  \31 FHTLT, 37 ZDVW => 1 FUEL"
                                , "6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, \
                                  \6 MZWV, 1 RJRHP => 6 FHTLT"
                                , "15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW"
                                , "13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, \
                                  \2 MZWV, 1 ZLQW => 1 ZDVW"
                                , "5 BMBT => 4 WPTQ"
                                , "189 ORE => 9 KTJDG"
                                , "1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP"
                                , "12 VRPVC, 27 CNZTR => 2 XDBXC"
                                , "15 KTJDG, 12 BHXH => 5 XCVML"
                                , "3 BHXH, 2 VRPVC => 7 MZWV"
                                , "121 ORE => 7 VRPVC"
                                , "7 XCVML => 6 RJRHP"
                                , "5 BHXH, 4 VRPVC => 5 LTCX"
                                ]
             , 2210736
             , 460664
             )
