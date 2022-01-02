module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec hiding (chunk)
import Text.Megaparsec.Char

import Data.Either (either, fromLeft, fromRight)
import qualified Data.Set as S

import AoC

type EnhancementAlgorithm = S.Set Int
type Coord = (Int, Int)

newtype Image = Image (Either (S.Set Coord) (S.Set Coord))

instance Show Image where
  show img = foldr (\((py,px),(cy,cx)) s ->
                     ( if cy == py
                       then replicate (cx-px-1) ' '
                         <> [pixel]
                       else replicate (cy-py) '\n'
                         <> replicate (cx-xm) ' '
                         <> [pixel]
                     ) <> s
                   )
                   "\n"
                   (zip ((ym,xm-1) : pixels) pixels)
    where
      pixel = leftRight '#' '.' img
      pixels = S.toList . unImage $ img
      (ym,yM,xm,xM) = border img

pixel :: Parser Char
pixel = char '.' <|> char '#'

line :: Parser [Int]
line = map fst . filter (snd) . zip [0..] . map (\p -> p == '#')
   <$> manyTill pixel eol

enhancementAlgorithm :: Parser EnhancementAlgorithm
enhancementAlgorithm = S.fromList <$> line

image :: Parser Image
image = Image
      . Left
      . S.fromList
      . concat
      . zipWith (\y -> map (\x -> (y,x))) [0..]
    <$> manyTill line eof

parser :: Parser (EnhancementAlgorithm, Image)
parser = do
  algo <- enhancementAlgorithm
  eol
  img <- image
  return (algo, img)

unImage :: Image -> S.Set Coord
unImage (Image (Left  pixels)) = pixels
unImage (Image (Right pixels)) = pixels

leftRight :: a -> a -> Image -> a
leftRight l r (Image e) = either (const l) (const r) e

neighbors :: Coord -> [Coord]
neighbors (y,x) = [(y+dy,x+dx) | dy <- [-1..1], dx <- [-1..1]]

index :: Image -> Coord -> Int
index img pixel = (\ns -> foldr (\n next i -> next (if n then 2*i + 1 else 2*i))
                                id
                                ns
                                0
                  )
                . map (yesNo . (`S.member` unImage img))
                . neighbors
                $ pixel
  where
    yesNo = leftRight id not img

enhance :: EnhancementAlgorithm -> Image -> Coord -> Bool
enhance alg img pixel = index img pixel `S.member` alg

border :: Image -> (Int, Int, Int, Int)
border img = (ym,yM,xm,xM)
  where
    img' = unImage img
    ym = fst . S.findMin $ img'
    yM = fst . S.findMax $ img'
    xs = S.map snd img'
    xm = S.findMin xs
    xM = S.findMax xs

enhanceImage :: EnhancementAlgorithm -> Image -> Image
enhanceImage alg img = Image
                     . toImage
                     . S.fromList
                     . filter (enhance alg img)
                     $ canvas
  where
    (ym,yM,xm,xM) = border img
    canvas = [(y,x) | y <- [ym-2..yM+2], x <- [xm-2..xM+2]]
    toImage | leftRight 0 (2^10 - 1) img `S.member` alg
            = Right . (S.fromAscList canvas S.\\)
            | otherwise = Left

part1 :: Parsed (EnhancementAlgorithm, Image) -> IO ()
part1 input = do
  let answer = S.size
             . unImage
             . (!! 2)
             . (\(alg, img) -> iterate (enhanceImage alg) img)
           <$> input
  printAnswer "Lit pixels after two enhancements: " answer

part2 :: Parsed (EnhancementAlgorithm, Image) -> IO ()
part2 input = do
  let answer = S.size
             . unImage
             . (!! 50)
             . (\(alg, img) -> iterate (enhanceImage alg) img)
           <$> input
  printAnswer "Lit pixels after fifty enhancements: " answer

main :: IO ()
main = do
  let day = "Day 20: Trench Map"
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
