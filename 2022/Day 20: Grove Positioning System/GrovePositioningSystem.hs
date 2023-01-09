module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.ExtendedReal as E
import qualified Data.Interval as Int
import qualified Data.IntegerInterval as II
import qualified Data.IntervalMap.Lazy as IntM
import qualified Data.IntMap as IM
import Data.Maybe (fromJust)

import AoC

import Data.List (stripPrefix)
import Debug.Trace

type ShiftMap = IntM.IntervalMap Integer Int

type Input = [Int]

parser :: Parser Input
parser = sepEndBy (signed integer) eol <* eof

modAdd :: (Integral a) => a -> a -> a -> a
modAdd mod a b = let r = (a + b) `rem` mod
                     r' | r < 0 = r + mod
                        | otherwise = r
                  in r'

coordinates :: Int -> IM.IntMap Int -> [Int]
coordinates zeroIndex mixed = map ( (mixed IM.!)
                                  . (\i -> modAdd cypherLength i zeroIndex)
                                  )
                                  [1000, 2000, 3000]
  where
    cypherLength = IM.size mixed

nTimes :: Int -> (a -> a) -> a -> a
nTimes rounds = foldr (.) id . replicate rounds

get0 :: Int -> ShiftMap -> Int
get0 index shifts = IntM.findWithDefault 0 (fromIntegral index) shifts

(<=..<=) :: Int -> Int -> II.IntegerInterval
l <=..<= u = E.Finite (fromIntegral l) II.<=..<= E.Finite (fromIntegral u)

(<..<=) :: Int -> Int -> II.IntegerInterval
l <..<= u = E.Finite (fromIntegral l) II.<..<= E.Finite (fromIntegral u)

(<=..<) :: Int -> Int -> II.IntegerInterval
l <=..< u = E.Finite (fromIntegral l) II.<=..< E.Finite (fromIntegral u)

whole :: Int -> Int -> IntM.IntervalMap Integer Int
whole width val = IntM.singleton (II.toInterval (0 <=..< width)) val

singleton :: II.IntegerInterval -> Int -> ShiftMap
singleton int = IntM.singleton (II.toInterval int)

trim :: Int.Interval Integer -> Int.Interval Integer
trim = II.toInterval . II.fromInterval

delete :: II.IntegerInterval -> ShiftMap
       -> ShiftMap
delete int = IntM.fromList
           . map (\(i,v) -> (trim i, v))
           . IntM.toAscList
           . IntM.delete (II.toInterval int)

shiftInterval :: Int -> II.IntegerInterval -> Int -> [II.IntegerInterval]
shiftInterval width int shift
  = let shift' = shift
        (E.Finite l, lB) = II.lowerBound' int
        (E.Finite u, uB) = II.upperBound' int
        l' = E.Finite (fromIntegral (modAdd width (fromIntegral l) shift'))
        u' = E.Finite (fromIntegral (modAdd width (fromIntegral u) shift'))
        (b0, bL)
          | l' <= u' = (lB, uB)
          | otherwise = (II.Closed, II.Closed)
        ints
          | l' <= u'
          = [II.interval (l', lB) (u', uB)]
          | otherwise
          = [ II.interval (0, b0) (u', uB)
            , II.interval (l', lB) (E.Finite (fromIntegral width), bL)
            ]
     in ints

inverse :: Int -> ShiftMap -> ShiftMap
inverse width intervals = IntM.fromListWith (+)
                        . concatMap
                            (\(int, val) ->
                               zip (map II.toInterval
                                        (shiftInterval width
                                                       (II.fromInterval int)
                                                       val
                                        )
                                   )
                                   (repeat (negate val))
                            )
                        . IntM.toAscList
                        $ intervals

filterEmpty :: ShiftMap -> ShiftMap
filterEmpty = IntM.fromList
            . map (\(i,v) ->
                    ( II.toInterval
                        (II.fromInterval i)
                    , v
                    )
                  )
            . IntM.toAscList

shiftsToIndexMap :: Input -> ShiftMap
                 -> (Int, IM.IntMap Int)
shiftsToIndexMap cypher shifts =
      (\(mZ,m) -> (fromJust mZ, m))
      .  foldl (\(mZ,m) (index,shift) ->
                 let index' = modAdd cypherLength
                                     index
                                     (get0 index shifts `rem` cypherLength)
                     mZ' | shift == 0 = Just index'
                         | otherwise = mZ
                  in (mZ', IM.insert index' shift m)
               )
               (Nothing, IM.empty)
      $ (zip [0..] cypher)
  where
    cypherLength = length cypher

shiftInts :: Int -> Int -> ShiftMap -> Int -> ShiftMap
shiftInts initialIndex shift shifts cypherLength
  = let index :: Int
        index = modAdd cypherLength
                       initialIndex
                       (get0 initialIndex shifts `rem` (cypherLength - 1))
        index' :: Int
        index' = modAdd cypherLength index (shift `rem` (cypherLength - 1))
        ints = let self | shift == 0 = IntM.empty
                        | otherwise
                        = singleton (II.singleton
                                       (fromIntegral index)
                                    )
                                    (shift `rem` (cypherLength - 1))
                   movedOver
                     | index < index'
                     = let s | shift > 0
                             = singleton (index <..<= index') (-1)
                             | otherwise
                             = delete (index <=..< index')
                                      (whole cypherLength 1)
                        in s
                     | index > index'
                     = let s | shift < 0
                             = singleton (index' <=..< index) 1
                             | otherwise
                             = delete (index' <..<= index)
                                      (whole cypherLength (-1))
                        in s
                     | otherwise = IntM.empty
                in IntM.union self movedOver
     in ints

-- | 1 2 -3 3 -2 0 4
-- | 2 1 -3 3 -2 0 4    [1,1]:-1 [0,0]:1
-- | 1 -3 2 3 -2 0 4    + [1,2]:-1 [0,0]:2 -- transform intervals with the
-- |                                       -- inverse of the current intervalmap
-- |                    [0,0]:0 [1,1]:1 [2,2]:-1
-- |                    [1,1]:1 [2,2]:-1
-- |                    + [0,0]:1 [5,6]:1 [1,1]:-3
-- |                    [0,0]:1 [1,1]:1 [2,2]:-4 [5,6]:1
-- | 4 1 2 3 -2 -3 0
-- | 1 2 3 -2 -3 0 4
-- |                    + [2,2]:3 [3,5]:-1
-- |                    [0,0]:1 [1,1]:4 [2,2]:-5 [3,4]:-1 [5,6]:1
-- | 4 1 3 -2 -3 2 0
-- mix :: Int -> Input -> (Int, IM.IntMap Int)
mix times cypher
  = shiftsToIndexMap cypher
  . nTimes times
           (\shiftMap ->
             foldl (\shifts (initialIndex, shift) ->
                     let shiftedShifts
                           = IntM.unions
                           . map
                               (\(int, val) ->
                                 let (_,overlap,_)
                                     -- ^ [0,1]:-1 [4,5]:2 -- [0,5]:x
                                     -- ^ [0,0]:x [-1,-1]:x [2,3]:x [6,7]:x
                                       = IntM.split int
                                                    (inverse cypherLength
                                                             shifts
                                                    )
                                     unshifted
                                       = filterEmpty
                                       $ IntM.difference
                                           (IntM.singleton int val)
                                           overlap
                                     shifted = filterEmpty
                                             $ IntM.map
                                                 (const val)
                                                 (inverse cypherLength
                                                          (filterEmpty overlap)
                                                 )
                                  in IntM.union unshifted shifted
                               )
                           . IntM.toAscList
                           $ shiftInts initialIndex shift shifts cypherLength
                         shifts' = filterEmpty
                                 $ IntM.unionWith (+) shifts shiftedShifts
                      in Debug.Trace.trace (
                         "\n" <>
                         show initialIndex <> " -> " <> show shift
                         <> "\n" <>
                         "shiftInts:" <>
                         ((fromJust . (>>= (stripPrefix "fromList")) . parseMaybe
                                       (sepBy anySingle
                                              (string "Finite" *> hspace
                                              <|> return ()
                                              )
                                        <* eof :: Parser String)
                          ) $ show (shiftInts initialIndex shift shifts cypherLength))
                         <> "\n" <>
                         "inverse:" <>
                         ((fromJust . (>>= (stripPrefix "fromList")) . parseMaybe
                                       (sepBy anySingle
                                              (string "Finite" *> hspace
                                              <|> return ()
                                              )
                                        <* eof :: Parser String)
                          ) $ show (inverse cypherLength shifts))
                         <> "\n" <>
                         "shiftedShifts:" <>
                         ((fromJust . (>>= (stripPrefix "fromList")) . parseMaybe
                                       (sepBy anySingle
                                              (string "Finite" *> hspace
                                              <|> return ()
                                              )
                                        <* eof :: Parser String)
                          ) $ show (shiftedShifts))
                         <> "\n" <>
                         "shifts':" <>
                         ((fromJust . (>>= (stripPrefix "fromList")) . parseMaybe
                                       (sepBy anySingle
                                              (string "Finite" *> hspace
                                              <|> return ()
                                              )
                                        <* eof :: Parser String)
                          ) $ show shifts')
                         <> "\n" <>
                         show (IM.elems . snd $ shiftsToIndexMap cypher shifts')
                         <> "\n" <>
                         show (map (`rem` 6) . IM.elems . snd $ shiftsToIndexMap cypher shifts')
                         )
                         $ shifts'
                   )
                   shiftMap
                   (zip [0..] cypher)
           )
  $ IntM.empty
  where
    cypherLength = length cypher

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . uncurry coordinates . mix 1 <$> input
  printAnswer "Sum of the coordinates: " answer

decryptionKey :: Int
decryptionKey = 811589153

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = {- sum . uncurry coordinates . -} mix 2 . map (* decryptionKey)
           <$> input
  printAnswer "Sum of decrypted coordinates: " answer

main :: IO ()
main = do
  let day = "Day 20: Grove Positioning System"
  input <- readInput day parser
  putStrLn ""
  part1 input
  part2 input
  --putStrLn ""
  --defaultMain [
  --    bgroup "AoC"
  --      [ bench "Part 1" $ nfIO (silence $ readInput day parser >>= part1)
  --      , bench "Part 2" $ nfIO (silence $ readInput day parser >>= part2)
  --      ]
  --  ]
