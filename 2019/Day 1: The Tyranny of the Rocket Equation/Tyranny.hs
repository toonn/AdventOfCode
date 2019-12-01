#! /usr/bin/env nix-shell
#! nix-shell -i runghc
#! nix-shell -p "haskellPackages.ghcWithPackages (ps: [ ps.containers ])"

module Tyranny where

main :: IO ()
main = (putStrLn . ("Total fuel for modules: " <>) . show =<< fuelRequirement)
  >> (putStrLn . ("Total fuel requirement: " <>) . show =<< totalFuel)
  where
    masses = (map read . lines) <$> readFile "./input.txt"
    fuelRequired m = (m `quot` 3) - 2
    fuelRequirement = sum <$> (map fuelRequired <$> masses)
    fuelReqs m = case fuelRequired m of
                   fuel | fuel > 0 -> fuel:fuelReqs fuel
                   _ -> []
    totalFuel = sum <$> (map (sum . fuelReqs) <$> masses)
