#! /usr/bin/env nix-shell
#! nix-shell -i runghc
#! nix-shell -p "haskellPackages.ghcWithPackages (ps: [ ps.containers ])"

module React where

import Data.Char

data Zipper = Z String String

instance Show Zipper where
  show (Z a b) = foldr (:) b a

main :: IO ()
main = do
  inertPolymer <- trigger . Z "" . init <$> readFile "./input"
  report "Inert polymer length: " length inertPolymer
  report "Shortest possible polymer: " optimize inertPolymer
  where
    report msg f = putStrLn . (msg <>) . show . f
    flipCase l = if isLower l then toUpper l else toLower l
    trigger (Z ls []) = ls
    trigger (Z [] (r:rs)) = trigger (Z [r] rs)
    trigger (Z (l:ls) (r:rs)) | l == flipCase r = trigger $ Z ls rs
                              | otherwise = trigger $ Z (r:l:ls) rs
    unitPairs = [[x, toUpper x] | x <- "abcdefghijklmnopqrstuvwxyz"]
    optimize iP = minimum $
      map (\uP -> length . trigger . Z "" . filter (not . (`elem` uP)) $ iP)
      unitPairs
