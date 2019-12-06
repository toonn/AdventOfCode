#! /usr/bin/env nix-shell
#! nix-shell -i runghc
#! nix-shell -p "haskellPackages.ghcWithPackages (ps: with ps; [ containers ])"

module UniversalOrbitMap where

import Text.ParserCombinators.ReadP
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Set as S

type Object = String
type COM = Object
type OrbitMap = M.Map Object (S.Set Object)

singleton :: Object -> Object -> OrbitMap
singleton center satellite = M.singleton center (S.singleton satellite)

union :: OrbitMap -> OrbitMap -> OrbitMap
union = M.unionWith S.union

satellites :: Object -> OrbitMap -> S.Set Object
satellites = M.findWithDefault S.empty

indirectOrbits' :: Object -> OrbitMap -> OrbitMap
indirectOrbits' com oM =
  foldr (\object oM' -> case indirectOrbits' object oM of
          iOM -> M.singleton com (satellites object iOM)
                 `union` iOM
                 `union` oM'
        )
        oM
        (satellites com oM)

indirectOrbits :: OrbitMap -> OrbitMap
indirectOrbits = indirectOrbits' "COM"

totalOrbits :: OrbitMap -> Int
totalOrbits = sum . map S.size . M.elems . indirectOrbits

nearestCommonObject :: OrbitMap -> OrbitMap -> Object -> Object
nearestCommonObject oM iOM o =
  foldr (\s o' -> case satellites s iOM of
          ss | (S.fromList ["SAN", "YOU"]) `S.isSubsetOf` ss ->
               nearestCommonObject oM iOM s
             | otherwise -> o'
        )
        o
        (satellites o oM)

transfers :: OrbitMap -> OrbitMap -> Object -> Object -> Int
transfers oM iOM a b =
  foldr (\o hops -> case (satellites o oM, satellites o iOM) of
          (ss, iSs) | (S.fromList [b]) `S.isSubsetOf` ss -> 1
                    | (S.fromList [b]) `S.isSubsetOf` iSs ->
                      1 + transfers oM iOM o b
                    | otherwise -> hops
        )
        (error "Object found and not found b.O")
        (satellites a oM)

minimumTransfers :: OrbitMap -> Int
minimumTransfers oM = sum (map (transfers oM iOM nCO) ["SAN", "YOU"])
  where
    iOM = indirectOrbits oM
    nCO = nearestCommonObject oM iOM "COM"

parseOrbit :: ReadP OrbitMap
parseOrbit = do
  center <- name
  char ')'
  satellite <- name
  return (singleton center satellite)
    where
    name = munch (`elem` (['A'..'Z'] ++ ['0'..'9']))

parseOrbitMap :: String -> OrbitMap
parseOrbitMap = fst . last . readP_to_S (do
  orbits <- many (skipSpaces >> parseOrbit)
  return (foldr union M.empty orbits)
  )

main :: IO ()
main = do
  orbitMap <- parseOrbitMap <$> readFile "input.txt"
  putStrLn . ("Total number of direct and indirect orbits: " <>) . show $
    totalOrbits orbitMap
  putStrLn . ("Minimum orbital transfers to SAN: " <>) . show $
    minimumTransfers orbitMap

example :: (String, Int)
example = ( intercalate "\n"
              [ "COM)B"
              , "B)C"
              , "C)D"
              , "D)E"
              , "E)F"
              , "B)G"
              , "G)H"
              , "D)I"
              , "E)J"
              , "J)K"
              , "K)L"
              , ""
              ]
          , 42
          )

exampleP2 :: (String, Int)
exampleP2 = ( intercalate "\n"
              [ "COM)B"
              , "B)C"
              , "C)D"
              , "D)E"
              , "E)F"
              , "B)G"
              , "G)H"
              , "D)I"
              , "E)J"
              , "J)K"
              , "K)L"
              , "K)YOU"
              , "I)SAN"
              , ""
              ]
          , 4
          )
