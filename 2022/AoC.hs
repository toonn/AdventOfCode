module AoC where

import Data.Void (Void)
import System.FilePath ((</>))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Paths_AoC2022

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

manhattan :: Integral a => (a,a) -> (a,a) -> a
manhattan (x,y) (x',y') = abs (x - x') + abs (y - y')

