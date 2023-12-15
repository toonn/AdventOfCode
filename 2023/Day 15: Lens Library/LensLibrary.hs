module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import qualified Data.IntMap as IM
import Data.List (break)
import Text.Read (readMaybe)

type Input = [String]

parser :: Parser Input
parser = sepBy1 (takeWhile1P (Just "Non comma or newline character")
                             (not . (`elem` ",\n"))
                )
                (char ',')
      <* eol <* eof

hASH :: Enum a => [a] -> Int
hASH string = foldr (\c more currentValue ->
                      more
                    . (`rem` 256)
                    . (17 *)
                    . (currentValue +)
                    . fromEnum
                    $ c
                    )
                    id
                    string
                    0

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . map hASH <$> input
  printAnswer "Total load on north support beams: " answer

followInstructions :: Input -> IM.IntMap [(String, Int)]
followInstructions instructions =
  foldr (\instruction more ->
          let (label,iRest) = break (`elem` "=-") instruction
              operation | iRest == "-" = foldr (\(l,fP) ->
                                                 let next | l == label
                                                          = id
                                                          | otherwise
                                                          = ((l,fP):)
                                                  in next
                                               )
                                               []
                        | Nothing <- (readMaybe (tail iRest) :: Maybe Int)
                        = error $ "Failing to parse: " <> iRest
                        | otherwise, Just focusPower <- readMaybe (tail iRest)
                        = \box -> foldr (\(l,fP) more insert ->
                                          let box' | l == label
                                                   = insert (more id)
                                                   | otherwise
                                                   = (l,fP) : more insert
                                           in box'
                                        )
                                        (\end -> end [])
                                        box
                                        ((label, focusPower):)
           in more
            . IM.alter (\mBox ->
                         let box | Nothing <- mBox = []
                                 | Just b <- mBox = b
                          in Just (operation box)
                       )
                       (hASH label)
        )
        id
        instructions
        mempty

focusingPower :: IM.IntMap [(String, Int)] -> IM.IntMap Int
focusingPower = IM.mapWithKey
  (\b lenses -> (b + 1) * sum (zipWith (*) [1..] (map snd lenses)))

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = sum . focusingPower . followInstructions <$> input
  printAnswer "Focusing power: " answer

main :: IO ()
main = do
  let day = "Day 15: Lens Library"
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
