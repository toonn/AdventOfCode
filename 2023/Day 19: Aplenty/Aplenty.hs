module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import Control.Arrow (second)
import Data.Char (isDigit, isLetter)
import qualified Data.IntegerInterval as II
import qualified Data.Interval as I
import qualified Data.IntervalSet as IS
import Data.List (elemIndex, intercalate, span)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)

type Rule = String
type Workflows = M.Map String [Rule]
type Part = [Int]

type Input = (Workflows, [Part])

braced :: Parser a -> Parser a
braced = between (char '{') (char '}')

commaSeparated :: Parser a -> Parser [a]
commaSeparated p = sepBy1 p (char ',')

workflow :: Parser (String, [Rule])
workflow = do
  name <- takeWhile1P (Just "Workflow name") isLetter
  rules <- braced
            (commaSeparated
              (takeWhile1P (Just "Rule character")
                           (\c -> c `elem` "<>:" || isDigit c || isLetter c)
              )
            )
  pure (name, rules)

part :: Parser Part
part = braced (commaSeparated (anySingle *> char '=' *> integer))

parser :: Parser Input
parser = (,)
       . M.fromList <$> sepEndBy1 workflow eol
      <* eol
     <*> sepEndBy1 part eol
      <* eof

apply :: String -> Part -> Maybe String
apply rule part =
  let (upToColon,rest) = span (/= ':') rule
      target | null rest = Just upToColon
             | (category:cOp:n) <- upToColon
             , Just i <- elemIndex category "xmas"
             , op <- if cOp == '<' then (<) else (>)
             , number <- (read n :: Int)
             , part !! i `op` number
             = Just (tail rest)
             | otherwise = Nothing
   in target

accepted :: Workflows -> String -> Part -> Bool
accepted workflows workflow part | rules <- workflows M.! workflow =
  foldr (\rule next ->
          let status | Just target <- apply rule part
                     = case target of
                         "A" -> True
                         "R" -> False
                         name -> accepted workflows name part
                     | otherwise = next
           in status
        )
        (error "No passing rule!")
        rules

accept :: Input -> [Part]
accept (workflows, parts) = filter (accepted workflows "in") parts

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = sum . map sum . accept <$> input
  printAnswer "Sum of accepted part ratings: " answer

splitRule :: Rule -> (String, String)
splitRule rule = foldr (\c more (cs, t) ->
                         let (cs', t') | c == ':'
                                       = (cs <> t <> ":", "")
                                       | otherwise
                                       = (cs, t <> [c])
                          in more (cs', t')
                       )
                       id
                       rule
                       ("","")

substitute :: Workflows -> Rule -> [Rule]
substitute workflows rule =
  let (condition, target) = splitRule rule
      rules | Just rs <- workflows M.!? target = map (condition <>) rs
            | otherwise = [rule]
   in rules

simplify :: [Rule] -> [Rule]
simplify rules | targets <- map (snd . splitRule) rules =
  foldr (\t more prevT ->
          let rules' | t == prevT = more t
                     | otherwise = rules
           in rules'
        )
        pure
        (tail targets)
        (head targets)

reduce :: Workflows -> Workflows
reduce workflows =
  M.foldrWithKey (\name rules more reduced ->
                   let rules' = simplify (concatMap (substitute reduced) rules)
                       reduced' = M.insert
                                    name
                                    rules'
                                    (M.map ( simplify
                                           . concatMap
                                               (substitute
                                                 (M.singleton name rules')
                                               )
                                           )
                                           reduced
                                    )
                    in more reduced'
                 )
                 id
                 workflows
                 M.empty

splitColon :: String -> [String]
splitColon s
  | null s = []
  | otherwise = uncurry (:) . second (splitColon . tail) . break (== ':') $ s

inverse :: String -> String
inverse (category:op:n) | number <- (read n :: Int) =
  let iOp | op == '<' = '>'
          | otherwise = '<'
      iNumber | op == '<' = number - 1
              | otherwise = number + 1
   in (category : iOp : show iNumber)

-- II.width is bugged, flipped lower and upper bound.
width :: I.Interval Integer -> Int
width i | I.null i = 0
        | (II.Finite l, lB) <- I.lowerBound' i
        , (II.Finite u, uB) <- I.upperBound' i
        = let l' | I.Open <- lB = l
                 | otherwise = l - 1
              u' | I.Open <- uB = u - 1
                 | otherwise = u
           in fromIntegral (u' - l')

distinctCombinations :: [Rule] -> Int
distinctCombinations rules =
  let (<=..<=) :: Integer -> Integer -> I.Interval Integer
      a <=..<= b = II.toInterval (II.Finite a II.<=..<= II.Finite b)
      (<=..<) :: Integer -> Integer -> I.Interval Integer
      a <=..< b = II.toInterval (II.Finite a II.<=..< II.Finite b)
      (<..<=) :: Integer -> Integer -> I.Interval Integer
      a <..<= b = II.toInterval (II.Finite a II.<..<= II.Finite b)
      wholeIntervals = M.fromList (map (\category ->
                                         ( category
                                         , IS.singleton (1 <=..<= 4000)
                                         )
                                       )
                                       "xmas"
                                  )
   in sum
    . map (product . map (sum . map width . IS.toList) . M.elems)
    $ mapMaybe (\rule ->
                 let (cs, target) = splitRule rule
                     conditions = splitColon cs
                     intervals | target == "R"
                               = Nothing
                               | otherwise
                               = Just
                               $ foldr (\c is ->
                                         let (category:cOp:n) = c
                                             number = (read n :: Integer)
                                             i | cOp == '<'
                                               = IS.singleton (1 <=..< number)
                                               | cOp == '>'
                                               = IS.singleton
                                                   (number <..<= 4000)
                                          in M.adjust (IS.intersection i)
                                                      category
                                                      is
                                       )
                                       wholeIntervals
                                       conditions
                  in intervals
               )
               rules

disconnect :: Workflows -> Workflows
disconnect = M.map (\rules ->
                     foldr (\rule more inverseCs ->
                             let (c, target) = splitRule rule
                                 rule' = intercalate ":" (inverseCs <> [rule])
                                 inverseCs' | null c
                                            = inverseCs
                                            | otherwise
                                            = inverseCs <> [inverse (init c)]
                              in rule' : more inverseCs'
                           )
                           (const [])
                           rules
                           []
                   )

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = distinctCombinations . (M.! "in") . reduce . disconnect . fst <$> input
  printAnswer "Distinct combinations of acceptable ratings: " answer

main :: IO ()
main = do
  let day = "Day 19: Aplenty"
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
