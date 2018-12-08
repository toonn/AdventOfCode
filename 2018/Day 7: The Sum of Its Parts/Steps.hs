#! /usr/bin/env nix-shell
#! nix-shell -i runghc
#! nix-shell -p "haskellPackages.ghcWithPackages (ps: [ ps.containers ])"

module Steps where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Text.ParserCombinators.ReadP

main :: IO ()
main = do
  (dependents, dependencies) <-
    orderMaps . map parseStep . lines <$> readFile "./input"
  report "Correct order: " order dependents
  report "Total time to completion (in s): " (show . totalTime) dependencies
  where
    report msg f = putStrLn . (msg <>) . f
    skipWord = skipSpaces >> munch (/= ' ') >> skipSpaces
    parseStep = fst . head . readP_to_S (skipWord >>
                                         get >>= \f ->
                                         count 5 skipWord >>
                                         get >>= \l ->
                                         return (f,l))
    findOrEmpty = M.findWithDefault S.empty
    alphaChronological oM a b | b `S.member` findOrEmpty a oM = LT
                              | a `S.member` findOrEmpty b oM = GT
                              | otherwise = compare a b
    transDeps s oM = let ds = findOrEmpty s oM in
      foldr (S.union . (`transDeps` oM)) ds ds
    orderMaps ss = foldr (\s (sM, mS) ->
      (M.insertWith S.union s (transDeps s sM) sM
      , M.insertWith S.union s (transDeps s mS) mS))
                        (foldr (\(f,l) (oM, mO)->
                                (M.insertWith S.union f (S.singleton l) oM
                                , M.insertWith S.union l (S.singleton f) mO))
                               (M.empty, M.empty)
                               ss)
                        ['A'..'Z']
    min oM x y = case alphaChronological oM x y of
                   LT -> x
                   _ -> y
    order' _ x Nothing = Just x
    order' min x (Just y) = Just (min x y)
    order'' oM os cs =
      case foldr (order' (min oM))
                 Nothing
                 (foldr S.delete cs (S.unions (M.elems oM))) of
        Nothing -> reverse os
        Just o -> order'' (M.delete o oM) (o:os) (S.delete o cs)
    order oM = order'' oM [] (S.fromDistinctAscList ['A'..'Z'])
    emptyKeys oM = M.foldrWithKey
      (\k v (eKs, oM') -> if S.null v
                             then (k:eKs, M.delete k oM')
                             else (eKs, oM'))
      ([], oM)
      oM
    tick' mT (c, k) (wB', oQ', oM', t')
      | c == mT = (wB', oQ', M.map (S.delete k) oM', t')
      | otherwise = ((c-mT, k):wB', oQ', oM', t')
    tick (wB, oQ, oM, time) = let minT = minimum (map fst wB) in
      foldr (tick' minT) ([], oQ, oM, time + minT) wB
    pInsert o k [] = o <> [k]
    pInsert o k (q:qs) = if snd k < snd q
                           then o <> (k:q:qs)
                           else pInsert (o <> [q]) k qs
    duration k = (60 + fromEnum k - 64, k)
    totalTime' ([], [], oM, time) | M.null oM = time
    totalTime' agg@(workBench, oQ, oM, time) = let wBL = length workBench in
      if wBL == 5
        then totalTime' (tick agg)
        else case emptyKeys oM of
          (eKs, oM') -> let (wB', oQ') = splitAt (5-wBL)
                              (foldr (pInsert [] . duration) oQ eKs)
                        in totalTime' (tick (workBench <> wB', oQ', oM', time))
    totalTime oM = case emptyKeys oM of
      (ks, oM') -> totalTime' ([], map duration (sort ks), oM', 0)
