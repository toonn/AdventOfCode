#! /usr/bin/env nix-shell
#! nix-shell -i runghc
#! nix-shell -p "haskellPackages.ghcWithPackages (ps: [ ps.containers ])"

module License where

import Text.ParserCombinators.ReadP

data License = L [License] [Int]

main :: IO ()
main = do
  license <- parseLicenseTree <$> readFile "./input"
  report "Sum of metadata: " (show . metaSum) license
  report "Value of root: " (show . value) license
  where
    report msg f = putStrLn . (msg <>) . f
    number = (read :: String -> Int) <$> munch (`elem` "0123456789")
    parseNode = number >>= \c ->
      skipSpaces >>
      number >>= \m ->
      skipSpaces >>
      count c parseNode >>= \children ->
      skipSpaces >>
      count m (number <* skipSpaces) >>= \metas ->
      skipSpaces >>
      return (L children metas)
    parseLicenseTree = fst . head . readP_to_S parseNode
    metaSum (L cs ms) = sum ms + sum (map metaSum cs)
    value (L [] ms) = sum ms
    value (L cs ms) = let e = length cs in
      foldr (\i v -> if i <= e then v + value (cs !! (i-1)) else v) 0 ms
