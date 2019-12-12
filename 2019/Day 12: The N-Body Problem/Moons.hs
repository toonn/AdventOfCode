#! /usr/bin/env nix-shell
#! nix-shell -i runghc
#! nix-shell -p "haskellPackages.ghcWithPackages (ps: with ps; [ containers ])"

module Moons where

import Control.Monad (guard)
import Data.List (intercalate, sortOn)
import qualified Data.Map as M
import qualified Data.Set as S

data Three = Three {x :: Int, y :: Int, z :: Int}
  deriving (Show, Read)
type Location = Three
type Velocity = Three
type Moon = (Location, Velocity)

zero :: Three
zero = Three 0 0 0

threeCompare :: Three -> Three -> (Ordering, Ordering, Ordering)
threeCompare (Three a b c) (Three a' b' c') =
  (a `compare` a', b `compare` b', c `compare` c')

alter :: Int -> Three -> (Ordering, Ordering, Ordering) -> Three
alter i (Three a b c) (a', b', c')
  | a' == LT = alter i (Three (a+i) b c) (EQ, b', c')
  | a' == GT = alter i (Three (a-i) b c) (EQ, b', c')
  | b' == LT = alter i (Three a (b+i) c) (EQ, EQ, c')
  | b' == GT = alter i (Three a (b-i) c) (EQ, EQ, c')
  | c' == LT = alter i (Three a b (c+i)) (EQ, EQ, EQ)
  | c' == GT = alter i (Three a b (c-i)) (EQ, EQ, EQ)
  | otherwise = Three a b c

left = alter 1
right = alter (-1)

move :: Three -> Three -> Three
move (Three a b c) (Three a' b' c') = Three (a+a') (b+b') (c+c')

gravity :: [Moon] -> [Moon]
gravity = foldr (\m -> uncurry (:)
                  . foldr (\(l',v') ((l,v), ms) -> let c = l `threeCompare` l'
                            in ((l,left v c), (l',right v' c):ms)
                          )
                          (m, [])
                )
                []

velocity :: [Moon] -> [Moon]
velocity = foldr (\(l, v) ms -> (l `move` v, v):ms) []

simulate :: Int -> [Moon] -> [Moon]
simulate 0 = id
simulate s = simulate (s - 1) . velocity . gravity

manhattan :: Three -> Int
manhattan (Three a b c) = sum (map abs [a,b,c])

energy :: Moon -> Int
energy (l,v) = product (map manhattan [l,v])

totalEnergy :: [Moon] -> Int
totalEnergy = sum . map energy

parseMoons :: String -> [Moon]
parseMoons s = zip (map ((read :: String -> Location) . three) (lines s))
                   (repeat zero)

three = ("Three " <>) . concatMap (\c -> let c' | c == '<' = "{"
                                                | c == '>' = "}"
                                                | c == '-' = " -"
                                                | otherwise = c:[]
                                    in c'
                                  )

main :: IO ()
main = do
  moons <- parseMoons <$> readFile "input.txt"
  putStrLn . ("Total energy after 1000 steps: " <>) . show $
    totalEnergy (simulate 1000 moons)

exampleP11 :: String
exampleP11 = intercalate "\n" [ "<x=-1, y=0, z=2>"
                              , "<x=2, y=-10, z=-7>"
                              , "<x=4, y=-8, z=8>"
                              , "<x=3, y=5, z=-1>"
                              ]

exampleP12 :: String
exampleP12 = intercalate "\n" [ "<x=-8, y=-10, z=0>"
                              , "<x=5, y=5, z=10>"
                              , "<x=2, y=-7, z=3>"
                              , "<x=9, y=-8, z=-3>"
                              ]
