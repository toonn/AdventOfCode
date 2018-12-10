#! /usr/bin/env nix-shell
#! nix-shell -i runghc
#! nix-shell -p "haskellPackages.ghcWithPackages (ps: [ ps.containers ])"

{-# LANGUAGE BangPatterns #-}


import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Sequence as S
import Text.ParserCombinators.ReadP

main :: IO ()
main = do
  (players, lastMarble) <- parseGame <$> readFile "./input"
  report "Winning score: " (show . winScore) (players, lastMarble)
  report "Winning score 100x more marbles: " (show . winScore)
    (players, lastMarble * 100)
  where
    report msg f = putStrLn . (msg <>) . f
    number = (read :: String -> Int) <$> munch (`elem` "0123456789")
    parseGame = fst . head . readP_to_S (number >>= \players ->
      count 5 (skipSpaces >> manyTill get (char ' ')) >>
      skipSpaces >>
      number >>= \lastMarble ->
      return (players, lastMarble))
    multiple x y = x `mod` y == 0
    turn places circle = case S.splitAt (places `mod` length circle) circle of
      (ls, rs) -> rs S.>< ls
    takeCurrent (m S.:<| ms) = (m, ms)
    makeMove players (!p, !pM, !circle, lastMod) n = (nP, nPM, nCircle, nMod)
      where nMultiple23       = (lastMod + 1) `multiple` 23
            nMod | nMultiple23 = 0
                 | otherwise   = lastMod + 1
            nP | p == players = 1
               | otherwise    = p + 1
            (c', nCircle) | nMultiple23 = takeCurrent $ turn (-7) circle
                          | otherwise = (0, n S.:<| turn 2 circle)
            nPM | nMultiple23 = M.insertWith (+) p (n + c') pM
                | otherwise   = pM
    pMap (a, b, c, _) = b
    winScore (players, lastMarble) = maximum . M.elems . pMap $
      foldl' (makeMove players) (1, M.empty, S.singleton 0, 0) [1..lastMarble]
