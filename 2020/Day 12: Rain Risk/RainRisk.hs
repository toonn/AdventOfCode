module Main where

import Criterion.Main
import qualified Data.Vector as V
import Data.Void (Void)
import System.FilePath ((</>))
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Paths_AoC2020

type Parser = Parsec Void String
type Parsed a = Either (ParseErrorBundle String Void) a

data Ship = Ship { x :: Integer
                 , y :: Integer
                 , direction :: Integer
                 }
data Action = E Integer | S Integer | W Integer | N Integer
            | F Integer | L Integer | R Integer
type Instructions = [Action]
data Waypointed = Waypoint { ship :: Ship
                           , waypoint :: Ship
                           }

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space hspace1 empty empty)

integer :: Parser Integer
integer = lexeme L.decimal

action :: Parser Action
action = do
  action <- oneOf "ESWNFLR"
  amount <- integer
  pure $ case action of
    'E' -> E amount
    'S' -> S amount
    'W' -> W amount
    'N' -> N amount
    'F' -> F amount
    'L' -> L amount
    'R' -> R amount

instructions :: Parser Instructions
instructions = sepEndBy action eol <* eof

readInput :: String -> IO (Parsed Instructions)
readInput day = do
  inputFile <- getDataFileName (day </> "input.txt")
  parse instructions inputFile
    <$> readFile inputFile

printAnswer :: Show a => String -> Parsed a -> IO ()
printAnswer question answer =
  either (putStrLn . errorBundlePretty)
         (putStrLn . (question <>) . show)
         answer

manhattan :: Ship -> Integer
manhattan ship = abs (x ship) + abs (y ship)

act :: Action -> (Ship -> Ship) -> Ship -> Ship
act (E amount) next ship = next $ ship {x = x ship + amount}
act (S amount) next ship = next $ ship {y = y ship - amount}
act (W amount) next ship = next $ ship {x = x ship - amount}
act (N amount) next ship = next $ ship {y = y ship + amount}
act (F amount) next ship | direction ship == 0   = act (E amount) next ship
                         | direction ship == 90  = act (S amount) next ship
                         | direction ship == 180 = act (W amount) next ship
                         | direction ship == 270 = act (N amount) next ship
act (L amount) next ship =
  next $ ship {direction = (direction ship - amount) `mod` 360}
act (R amount) next ship =
  next $ ship {direction = (direction ship + amount) `mod` 360}

follow :: (Action -> (a -> a) -> a -> a) -> a -> Instructions -> a
follow act ship instructions = foldr act id instructions ship

part1 :: Parsed Instructions -> IO ()
part1 input = do
  let answer = manhattan . follow act (Ship 0 0 0) <$> input
  printAnswer "Manhattan distance to destination: " answer

rotate :: Integer -> Ship -> Ship
rotate amount (Ship x y d) = case amount `mod` 360 of
  90  -> Ship y (-x) d
  180 -> Ship (-x) (-y) d
  270 -> Ship (-y) x d

act2 :: Action -> (Waypointed -> Waypointed) -> Waypointed -> Waypointed
act2 (F amount) next (Waypoint (Ship x y d) wp@(Ship dx dy _)) =
  next $ Waypoint (Ship (x + amount * dx) (y + amount * dy) d) wp
act2 (L amount) next (Waypoint ship wp) =
  next $ Waypoint ship (rotate (-amount) wp)
act2 (R amount) next (Waypoint ship wp) =
  next $ Waypoint ship (rotate amount wp)
act2 a next wp = next $ wp { waypoint =
  act a id (waypoint wp) }


part2 :: Parsed Instructions -> IO ()
part2 input = do
  let answer = manhattan
             . ship . follow act2 (Waypoint (Ship 0 0 0) (Ship 10 1 0))
           <$> input
  printAnswer "Manhattan distance to actual destination: " answer

main :: IO ()
main = do
  let day = "Day 12: Rain Risk"
  input <- readInput day
  putStrLn ""
  part1 input
  part2 input
  putStrLn ""
  defaultMain [
      bgroup "AoC"
        [ bench "Part 1" $ nfIO (silence $ readInput day >>= part1)
        , bench "Part 2" $ nfIO (silence $ readInput day >>= part2)
        ]
    ]
