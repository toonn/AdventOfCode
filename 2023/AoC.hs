module AoC where

import Control.Arrow ((***))
import Control.Monad (join)
import Data.Void (Void)
import qualified Data.Map as M
import System.FilePath ((</>))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Paths_AoC2023

type Parser = Parsec Void String
type Parsed a = Either (ParseErrorBundle String Void) a

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space hspace1 empty empty)

integer :: Num a => Parser a
integer = lexeme L.decimal

signed :: Num a => Parser a -> Parser a
signed = L.signed (L.space empty empty empty)

readInput :: String -> Parser a -> IO (Parsed a)
readInput day parser = do
  inputFile <- getDataFileName (day </> "input.txt")
  parse parser inputFile
    <$> readFile inputFile

printAnswer :: Show a => String -> Parsed a -> IO ()
printAnswer question answer =
  either (putStrLn . errorBundlePretty)
         (putStrLn . (question <>) . show)
         answer

both :: (a -> b) -> (a,a) -> (b,b)
both = join (***)

type YX = (Int, Int)

foldYX :: [[a]] -> M.Map YX a
foldYX rows = M.fromAscList
            $ foldr (\row more y ->
                      foldr (\a more x -> ((y, x), a) : more (x + 1))
                            (const (more (y + 1)))
                            row
                            0
                    )
                    (const [])
                    rows
                    0

hamming :: (Eq a, Num d) => [a] -> [a] -> d
hamming as bs = fromIntegral . length . filter id $ zipWith (/=) as bs

manhattan :: Integral a => (a,a) -> (a,a) -> a
manhattan (x,y) (x',y') = abs (x - x') + abs (y - y')

nTimes :: Int -> (a -> a) -> a -> a
nTimes rounds = foldr (.) id . replicate rounds
