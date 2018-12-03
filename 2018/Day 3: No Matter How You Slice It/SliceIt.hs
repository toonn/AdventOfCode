#! /usr/bin/env nix-shell
#! nix-shell -i runghc
#! nix-shell -p "haskellPackages.ghcWithPackages (ps: [ ps.containers ])"

module SliceIt where

import Text.ParserCombinators.ReadP
import qualified Data.Set as S

main :: IO ()
main = do
  claims  <- map parseClaim . lines <$> readFile "./input"
  let (_, overlappingSqs, overlappingIDs) = overlapping claims
  report "Overlapping square inches: " overlap overlappingSqs
  report "Only succesful claim: "
         (`notOverlapping` (overlappingSqs, overlappingIDs))
         claims
  where
    report msg f = putStrLn . (msg <>) . show . f
    number = (read :: String -> Integer) <$> munch1 (`elem` "0123456789")
    claim _ [id, x, y, w, h] = (id, (,) <$> [x+1..x+w] <*> [y+1..y+h] )
    claim s xs = error $ unwords
      [ "Failed to parse exactly an `id`, `x`, `y`, `w` and `h`"
      , s, show xs ]
    parseClaim s = fst . last . readP_to_S
      (char '#' >>
       sepBy number (skipMany1 (satisfy (`elem` " @,:x"))) >>= \xs ->
       return (claim s xs)
      ) $ s
    collect iD sqIn (once, multiple, iDs)
      | sqIn `S.member` once =
        (once, sqIn `S.insert` multiple, iD `S.insert` iDs)
      | otherwise =
        (sqIn `S.insert` once, multiple, iDs)
    overlapping' (iD, sqIns) over = foldr (collect iD) over sqIns
    overlapping = foldr overlapping' (S.empty, S.empty, S.empty)
    overlap = S.size
    inOverlapping sqs ms = foldr ((||) . (`S.member` ms)) False sqs
    notOverlapping ((iD, sqs):iDs) o@(overlappingSqs, overlappingIDs)
      | iD `S.member` overlappingIDs = iDs `notOverlapping` o
      | sqs `inOverlapping` overlappingSqs = iDs `notOverlapping` o
      | otherwise = iD
    notOverlapping [] _ = error "No non overlapping claims."
