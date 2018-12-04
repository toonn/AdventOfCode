#! /usr/bin/env nix-shell
#! nix-shell -i runghc
#! nix-shell -p "haskellPackages.ghcWithPackages (ps: [ ps.containers ])"

module SneakIn where

import Text.ParserCombinators.ReadP
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.List (sortOn)

data Event = ID Integer | W | F deriving (Show)

main :: IO ()
main = do
  summary <- summarize . sortOn fst . map parseRecord . lines <$> readFile "./input"
  report "mostAsleepID * mostFreqMinute: " (fromJust . mAmF) summary
  report "mostFreqID * mostFreqMinute: " (fromJust . mFmF) summary
  where
    report msg f = putStrLn . (msg <>) . show . f
    number = (read :: String -> Integer) <$> munch (`elem` "0123456789")
    date = manyTill (satisfy (/= '-')) (char '-') >>
      number >>= \month -> char '-' >>
      number >>= \day -> skipSpaces >>
      number >>= \hour -> char ':' >>
      number >>= \minute ->
      return (month, day, hour, minute)
    event = (ID <$> (string "Guard #" >> number))
      +++ (char 'w' >> return W) +++ (char 'f' >> return F)
    parseRecord = fst . head . readP_to_S
      (between (char '[') (char ']') date >>= \d -> skipSpaces >>
       event >>= \e ->
       return (d, e))
    insertEvent (_, _, eMap) (_ , ID iD) =
      (Just iD, Nothing, M.insertWith (flip const) iD M.empty eMap)
    insertEvent (mID@(Just _), _, eMap) ((_,_, _, m), F) =
      (mID, Just m, eMap)
    insertEvent (mID@(Just iD), Just m', eMap) ((_,_, _, m), W) =
      (mID, Nothing,
       M.adjust (\x -> foldr (\k  -> M.insertWith (+) k 1) x [m'..m-1]) iD eMap)
    insertEvent x e = error $ show e <> show x
    thd (_, _, x) = x
    summarize = thd . foldl insertEvent (Nothing, Nothing, M.empty)
    maxKV k v (Just max) = if v > snd max then Just (k, v) else Just max
    maxKV k v Nothing = Just (k, v)
    maxKey = fmap fst . M.foldrWithKey maxKV Nothing
    mostAsleepID = maxKey . M.map (sum . M.elems)
    mostFrequentMinute iD = maxKey . (M.! iD)
    mAmF m = let mAID = mostAsleepID m in
      (*) <$> mAID <*> (mAID >>= \iD -> mostFrequentMinute iD m)
    insertMax k mMap acc = case M.elems mMap of
      [] -> acc
      es -> M.insert k (maximum es) acc
    mostFreqID = maxKey . M.foldrWithKey insertMax M.empty
    mFmF m = let mFID = mostFreqID m in
      (*) <$> mFID <*> (mFID >>= \iD -> mostFrequentMinute iD m)
