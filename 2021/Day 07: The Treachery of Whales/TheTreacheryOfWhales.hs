module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.IntMap as M

import AoC

type Crabs = M.IntMap Int

parser :: Parser Crabs
parser = foldr (\days -> M.insertWith (const (+ 1)) days 1) M.empty
     <$> sepBy integer (char ',') <* (eol *> eof)

leftLeaningMedian :: Crabs -> Int
leftLeaningMedian crabs = M.foldrWithKey (\k freq next total ->
                                           if total + freq >= place
                                           then k
                                           else next (total + freq)
                                         )
                                         (error "No median found!")
                                         crabs
                                         0
  where
    place = (sum crabs) `quot` 2

fuelToAlign :: Int -> Crabs -> Int
fuelToAlign location = M.foldrWithKey
  (\loc number fuel ->
    abs (location - loc) * number + fuel
  )
  0

part1 :: Parsed Crabs -> IO ()
part1 input = do
  let answer = (\crabs -> fuelToAlign (leftLeaningMedian crabs) crabs) <$> input
  printAnswer "Fuel to align to optimal position: " answer

average :: Crabs -> Int
average crabs = round
  ( fromIntegral (M.foldrWithKey (\k freq total -> k * freq + total) 0 crabs)
  / fromIntegral (sum crabs)
  )

triangularNumber :: Int -> Int
triangularNumber n = round (n' * (n' + 1) / 2)
  where n' = fromIntegral n

rampingFuelToAlign :: Int -> Crabs -> Int
rampingFuelToAlign location = M.foldrWithKey
  (\loc number fuel ->
    triangularNumber (abs (location - loc)) * number + fuel
  )
  0

descend :: Int -> Crabs -> Int
descend location crabs = go l f l' f'
  where
    locationFuel = rampingFuelToAlign location crabs
    location' = location - 1
    locationFuel' = rampingFuelToAlign location' crabs
    (l,f,l',f') | locationFuel < locationFuel' = ( location'
                                                 , locationFuel'
                                                 , location
                                                 , locationFuel
                                                 )
                | otherwise = ( location
                              , locationFuel
                              , location'
                              , locationFuel'
                              )

    go loc fuel loc' fuel' | fuel < fuel' = fuel
                           | otherwise = go loc' fuel' loc'' fuel''
      where
        loc'' = 2 * loc' - loc
        fuel'' = rampingFuelToAlign loc'' crabs

part2 :: Parsed Crabs -> IO ()
part2 input = do
  let answer = (\crabs -> descend (average crabs) crabs) <$> input
  printAnswer "Fuel to align according to crab engineering: " answer

main :: IO ()
main = do
  let day = "Day 07: The Treachery of Whales"
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
