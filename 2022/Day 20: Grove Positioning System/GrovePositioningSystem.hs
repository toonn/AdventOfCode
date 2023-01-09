module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.IntMap as IM

import AoC

type DoublyLinkedMap = IM.IntMap (Int, Int, Int)

type Input = [Int]

parser :: Parser Input
parser = sepEndBy (signed integer) eol <* eof

nTimes :: Int -> (a -> a) -> a -> a
nTimes rounds = foldr (.) id . replicate rounds

coordinates :: DoublyLinkedMap -> [Int]
coordinates mixed
  = foldr (\index more next shifts mMaxIndex indices ->
              let (_,shift,right) = mixed IM.! next
                  shifts' = IM.insert index shift shifts
                  indices'
                    | shift == 0 = map ((`rem` IM.size mixed) . (+ index))
                                       [1000, 2000, 3000]
                    | otherwise = indices

                  mMaxIndex' | not (null indices') = Just (maximum indices')
                             | otherwise = mMaxIndex

                  cs | Just maxIndex <- mMaxIndex' , index >= maxIndex
                     = map (shifts' IM.!) indices'
                     | otherwise = more right shifts' mMaxIndex' indices'
               in cs
          )
          (error "No shift of 0 found")
          [0..IM.size mixed - 1]
          0
          IM.empty
          Nothing
          []

mix :: Int -> Input -> DoublyLinkedMap
mix times cypher
  = nTimes times
      (\shiftMap ->
        foldl
          (\sM index ->
            let (left, shift, right) = sM IM.! index
                smallShift = shift `rem` (cypherLength - 1)
                shift' | abs smallShift <= halfLength = smallShift
                       | smallShift < 0 = (cypherLength - 1) + smallShift
                       | otherwise = smallShift - (cypherLength - 1)
                (left', right') | shift' < 0
                                = foldl (\(p,_) _ ->
                                          let (p',_,_) = sM IM.! p
                                           in (p',p)
                                        )
                                        (left, right)
                                        [1..abs shift']
                                | shift' > 0
                                = foldl (\(_,n) _ ->
                                          let (_,_,n') = sM IM.! n
                                           in (n,n')
                                        )
                                        (left, right)
                                        [1..shift']
                                | shift' == 0 = (left, right)
                sM' = IM.adjust (const (left', shift, right')) index
                    . IM.adjust (\(l,s,_) -> (l, s, index)) left'
                    . IM.adjust (\(_,s,r) -> (index, s, r)) right'
                    . IM.adjust (\(l,s,_) -> (l, s, right)) left
                    . IM.adjust (\(_,s,r) -> (left, s, r)) right
                    $ sM
             in sM'
          )
          shiftMap
          [0..cypherLength - 1]
      )
      cypherMap
  where
    cypherLength = length cypher
    halfLength = let (q,r) = cypherLength `quotRem` 2
                     h | r == 0 = q
                       | otherwise = q + 1
                  in h
    cypherMap = IM.fromAscList
              . map (\(index, shift) ->
                      (index, (loop (index - 1), shift, loop (index + 1)))
                    )
              . zip [0..]
              $ cypher
    loop i = let i' = i `rem` cypherLength
                 i'' | i' < 0 = i' + cypherLength
                     | otherwise = i'
              in i''

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . coordinates . mix 1 <$> input
  printAnswer "Sum of the coordinates: " answer

decryptionKey :: Int
decryptionKey = 811589153

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = sum . coordinates . mix 10 . map (* decryptionKey)
           <$> input
  printAnswer "Sum of decrypted coordinates: " answer

main :: IO ()
main = do
  let day = "Day 20: Grove Positioning System"
  input <- readInput day parser
  putStrLn ""
  part1 input
  part2 input
  putStrLn ""
  defaultMain [
      bgroup "AoC"
        [ bench "Part 1" $ nfIO (silence $ readInput day parser >>= part1)
        , bench "Part 2" $ nfIO (silence $ readInput day parser >>= part2)
        ]
    ]
