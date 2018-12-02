#! /usr/bin/env nix-shell
#! nix-shell -i runghc
#! nix-shell -p "haskellPackages.ghcWithPackages (ps: [ ps.containers ])"

module ChronalCalibration where

import qualified Data.Set as P

main :: IO ()
main = (putStrLn . ("Total frequency drift: " <>) . show =<< drift)
  >> (putStrLn . ("First repeated frequency: " <>) . show =<< repeat)
  where
    shifts = map (read . filter (/= '+')) . lines <$> readFile "./input"
    drift = sum <$> shifts
    repeat = firstRepeat P.empty . scanl (+) 0 . cycle <$> shifts
    firstRepeat xs (y:ys) = if y `P.member` xs then y else firstRepeat (P.insert y xs) ys
