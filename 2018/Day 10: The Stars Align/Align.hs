#! /usr/bin/env nix-shell
#! nix-shell -i runghc
#! nix-shell -p "haskellPackages.ghcWithPackages (ps: [ ps.containers ])"

module Align where

import Data.Char (isDigit)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Text.ParserCombinators.ReadP

main :: IO ()
main = do
  stars <- map parseStar . lines <$> readFile "./input"
  report "Message in the stars: " message stars
  report "Time to message: " (show . timeToWait) stars
  where
    report msg f = putStrLn . (msg <>) . f
    lt = char '<'
    gt = char '>'
    number = (read :: String -> Int) <$> munch (\c -> c == '-' || isDigit c)
    vec = skipSpaces >>
      number >>= \x ->
      char ',' >> skipSpaces >>
      number >>= \y -> return (x, y)
    parseStar = fst . head . readP_to_S (manyTill get lt >>
      vec >>= \coord ->
      manyTill get lt >>
      vec >>= \velocity ->
      return (coord, velocity))
    minyMaxY' ((_, y), (_, vy)) Nothing = Just (y, vy, y, vy)
    minyMaxY' ((_, y), (_, vy)) acc@(Just (ym, vym, yM, vyM))
      | y < ym    = Just (y, vy, yM, vyM)
      | y > yM    = Just (ym, vym, y, vy)
      | otherwise = acc
    minyMaxY = foldr minyMaxY' Nothing
    move steps ((x, y), v@(vx, vy)) = ((x + steps * vx, y + steps * vy), v)
    within8 (m, _, mM, _) = abs (m - mM) <= 9
    starsWithin8' (ym, vym, yM, vyM) =
      max 0 (abs (ym - yM) - 9) `div` abs (vym - vyM)
    -- Stop criterion breaks down if stars cluster close enough together
    -- without forming a message.
    starsWithin8 stars
      | within8 ymM = stars
      | otherwise   = starsWithin8 $ map (move steps) stars
      where
        ymM = fromJust $ minyMaxY stars
        steps = starsWithin8' ymM
    render stars = '\n' : concat [[chr (x,y) | x <- [xm..xM]] <> "\n"
                                  | y <- [ym..yM]]
      where
        chr x = if x `elem` map fst stars then '#' else ' '
        (xm, ym, xM, yM) = foldr
          (\((x,y),_) (m,m',mM,mM') -> (min x m, min y m', max x mM, max y mM'))
          (1000000,1000000,-1000000,-1000000)
          stars
    -- Features based on guesses of AoC's letter representation.
    -- Some characters can not be distinguished without negative features, R
    -- has all the features P needs to have. (It has some it should not.)
    charFeatures = M.fromAscList
      [ ('A', [ (0,9), (5,9), (0,4), (3,4), (5,4), (3,0) ])
      , ('B', [ (0,9), (3,9), (0,4), (3,4), (5,4), (0,0), (3,0), (5,0) ])
      , ('C', [ (3,9), (0,4), (3,0), (4,9) ])
      , ('D', [ (0,9), (3,9), (0,4), (0,0), (3,0) ])
      , ('E', [ (0,9), (3,9), (5,9), (0,4), (3,4), (5,4), (0,0), (3,0), (5,0) ])
      , ('F', [ (0,9), (0,4), (3,4), (5,4), (0,0), (3,0), (5,0) ])
      , ('G', [ (3,9), (5,9), (0,5), (3,5), (5,5), (3,0) ])
      , ('H', [ (0,9), (5,9), (0,4), (3,4), (5,4), (0,0), (5,0) ])
      , ('I', [ (3,9), (3,4), (3,0), (4,9) ])
      , ('J', [ (3,9), (5,4), (5,0) ])
      , ('K', [ (0,9), (5,9), (0,4), (0,0), (5,0) ])
      , ('L', [ (0,9), (3,9), (5,9), (0,4), (0,0) ])
      , ('M', [ (0,9), (5,9), (0,4), (5,4), (0,0), (5,0) ])
      , ('N', [ (0,9), (5,9), (0,4), (2,4), (5,4), (0,0), (5,0) ])
      , ('O', [ (3,9), (0,4), (5,4), (5,3) ])
      , ('P', [ (0,9), (0,4), (3,4), (0,0), (3,0), (5,2) ])
      , ('Q', [ (3,9), (5,9), (0,4), (5,4), (5,3) ])
      , ('R', [ (0,9), (5,9), (0,4), (3,4), (0,0), (3,0), (5,2) ])
      , ('S', [ (0,9), (3,9), (2,4), (3,0), (5,0) ])
      , ('T', [ (3,9), (3,4), (0,0), (3,0), (5,0) ])
      , ('U', [ (3,9), (0,4), (5,4), (0,0), (5,0) ])
      , ('V', [ (3,9), (0,0), (5,0) ])
      , ('W', [ (3,4), (0,0), (3,0), (5,0) ])
      , ('X', [ (0,9), (5,9), (3,4), (0,0), (5,0) ])
      , ('Y', [ (3,9), (3,4), (0,0), (5,0) ])
      , ('Z', [ (0,9), (3,9), (5,9), (3,4), (0,0), (3,0), (5,0) ])
      ]
    recognizeChar stars = M.foldrWithKey
      (\k fs c -> if all (`elem` stars) fs then k else c)
      '.'
      charFeatures
    splitChars stars = [[(x-x',y-ym) | x <- [x'..x'+5], y <- [ym..yM]
                                     , (x,y) `elem` map fst stars]
                        | x' <- [xm,xm+8..xM]]
      where
        (xm, ym, xM, yM) = foldr
          (\((x,y),_) (m,m',mM,mM') -> (min x m, min y m', max x mM, max y mM'))
          (1000000,1000000,-1000000,-1000000)
          stars
    message = (\ss ->
      render ss <> ('\n':(map recognizeChar . splitChars $ ss))) . starsWithin8
    timeToWait = starsWithin8' . fromJust . minyMaxY
