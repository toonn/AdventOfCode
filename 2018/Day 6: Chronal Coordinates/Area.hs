#! /usr/bin/env nix-shell
#! nix-shell -i runghc
#! nix-shell -p "haskellPackages.ghcWithPackages (ps: [ ps.containers ])"

module Area where

import qualified Data.Map as M
import qualified Data.Set as S
import Text.ParserCombinators.ReadP

main :: IO ()
main = do
  coords <- map parseCoord . lines <$> readFile "./input"
  let bs = bounds coords
  report "Largest finite area: " (largest bs) coords
  report "Safe region area: " (safest bs) coords
  where
    report msg f = putStrLn . (msg <>) . show . f
    number = (read :: String -> Integer) <$> munch (`elem` "0123456789")
    parseCoord = fst . head . readP_to_S
      (number >>= \x -> string ", " >> number >>= \y -> return (x,y))
    updateBounds (x,y) (xm,ym,xM,yM) = case (x < xm, y < ym, x > xM, y > yM) of
      (True, True, _, _) -> (x ,y ,xM,yM)
      (True, _, _, True) -> (x ,ym,xM,y )
      (_, True, True, _) -> (xm,y ,x ,yM)
      (_, _, True, True) -> (xm,ym,x ,y )
      (True, _, _, _) -> (x ,ym,xM,yM)
      (_, _, True, _) -> (xm,ym,x ,yM)
      (_, True, _, _) -> (xm,y ,xM,yM)
      (_, _, _, True) -> (xm,ym,xM,y )
      (_, _, _, _) -> (xm,ym,xM,yM)
    bounds ((x,y):cs) = foldr updateBounds (x, y, x, y) cs
    (x,y) `onEdge` (xm,ym,xM,yM) = x == xm || y == ym || x == xM || y == yM
    manhattan (x,y) (x',y') = abs (x-x') + abs (y-y')
    minD p c' (dm, cls) = let d = manhattan p c' in
      case d `compare` dm of LT -> (d , [c']  )
                             EQ -> (dm, c':cls)
                             GT -> (dm, cls   )
    closest (c:cs) p = snd $ foldr (minD p) (manhattan p c, [c]) cs
    edgeNArea cs bs p (oE, cnts) = case (p `onEdge` bs, cs `closest` p) of
      (True, [c]) -> (S.insert c oE        , M.insertWith (+) c 1 cnts)
      (_   , [c]) -> (oE                   , M.insertWith (+) c 1 cnts)
      (_   , _  ) -> (oE                   , cnts                     )
    areas cs bs@(xm,ym,xM,yM) = foldr (edgeNArea cs bs)
                                      (S.empty, M.empty)
                                      [(x,y) | x <- [xm..xM], y <- [ym..yM]]
    largest bs coords = case areas coords bs of
      (edgeSet, areaMap) -> maximum . M.elems $ foldr M.delete areaMap edgeSet
    center (xm,ym,xM,yM) = (xm+((xM-xm) `div` 2),ym+((yM-ym) `div` 2))
    p +/- 0 = [p]
    (x,y) +/- d = [(x+x',y+y') | x' <- [-d..d]
                               , let dy = d - abs x'
                               , y' <- if dy == 0 then [0] else [-dy, dy]]
    totalDistance p = foldr ((+) . manhattan p) 0
    accrue (0, acc) _ = (0, acc)
    accrue (_, acc) v = (v, acc + v)
    safest bs cs = snd $ foldl (\x -> accrue x .
      foldr ((+) . (\p -> if totalDistance p cs < 10000 then 1 else 0)) 0)
      (1,0)
      [center bs +/- d | d <- [0..10000]]
