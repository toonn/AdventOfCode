#! /usr/bin/env nix-shell
#! nix-shell -i runghc
#! nix-shell -p "haskellPackages.ghcWithPackages (ps: [])"

module ChronalCalibration where

main :: IO ()
main = (putStrLn . ("Total frequency drift: " <>) . show =<< drift)
  >> (putStrLn . ("First repeated frequency: " <>) . show =<< repeat)
  where
    shifts = map (read . filter (/= '+')) . lines <$> readFile "./input"
    drift = sum <$> shifts
    repeat = firstRepeat [] . scanl (+) 0 . cycle <$> shifts
    firstRepeat xs (y:ys) = if y `elem` xs then y else firstRepeat (y:xs) ys
