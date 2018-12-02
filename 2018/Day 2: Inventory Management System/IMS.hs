#! /usr/bin/env nix-shell
#! nix-shell -i runghc
#! nix-shell -p "haskellPackages.ghcWithPackages (ps: [ ps.containers ])"

module IMS where

import qualified Data.Map as M

main :: IO ()
main = readFile "./input" >>= \boxIDs ->
  report "Checksum: " checksum boxIDs
  >> report "Common letters: " common boxIDs
  where
    report msg f = putStrLn . (msg <>) . show . f . lines
    tupAdd (a, b) (c, d) = (a + c, b + d)
    freqs = M.elems . foldr (\k -> M.insertWith (+) k 1) M.empty
    iN x = fromEnum . (x `elem`)
    both (a, b) f x = (f a x, f b x)
    checkID = ((2, 3) `both` iN) . freqs
    checksum = uncurry (*) . foldr (tupAdd . checkID) (0,0)
    dist (x:xs) (y:ys) = dist xs ys + if x == y then 0 else 1
    dist [] [] = 0
    similar' xs x (y:ys) = if dist x y == 1 then (x, y) else similar' xs x ys
    similar' xs _ [] = similar xs
    similar (x:xs) = similar' xs x xs
    similar [] = ("No similar strings.", "No similar strings.")
    common = (\(a,b) -> filter (`elem` a) b) . similar
