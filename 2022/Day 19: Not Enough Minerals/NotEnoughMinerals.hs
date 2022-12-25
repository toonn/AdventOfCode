module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.PQueue.Min as PQ

import AoC

data Resource = Ore | Clay | Obsidian | Geode deriving (Eq, Ord, Show)

type Resources = M.Map Resource Int
type Robot = Resource
type Robots = Resources

type Blueprint = M.Map Resource Resources

type Input = IM.IntMap Blueprint

resource :: Parser Resource
resource = choice
         . map (\(s,r) -> string s *> pure r)
         $ [ ("ore", Ore)
           , ("clay", Clay)
           , ("obsidian", Obsidian)
           , ("geode", Geode)
           ]

robot :: Parser (Robot, Resources)
robot = do
  lexeme (string "Each")
  rob <- resource
  hspace
  lexeme (string "robot costs")
  ress <- M.fromList <$> sepBy (do qty <- integer
                                   hspace
                                   res <- resource
                                   pure (res,qty)
                               )
                               (hspace *> lexeme (string "and"))
  lexeme (char '.')
  pure (rob, ress)

blueprint :: Parser (Int, Blueprint)
blueprint = do
  lexeme (string "Blueprint")
  iD <- integer
  lexeme (char ':')
  bp <- M.fromList <$> sepBy robot hspace
  pure (iD, bp)

parser :: Parser Input
parser = IM.fromAscList <$> sepEndBy (blueprint) eol <* eof

get0 :: Ord a => a -> M.Map a Int -> Int
get0 = M.findWithDefault 0

sufficient :: Resources -> Bool
sufficient = M.foldr (\qty -> (qty >= 0 &&)) True

produceRobots :: Blueprint -> Resources -> Robots -> Int -> [(Int, Resources, Robots)]
produceRobots blueprint resources robots minutes
  | minutes == 0 = error "Can't produce robots without time"
  | otherwise
  = ( 0
    , M.unionWith (+) resources (M.map (* minutes) robots)
    , robots
    ) : mapMaybe (\robot ->
                   let proj = projected robot robots resources
                       mRobot
                         | robot == Geode || minutes - proj >= 3
                         , minutes - proj >= 1
                         = Just ( minutes - proj - 1
                                , M.unionWith (+)
                                              (after robot)
                                              (M.map (* (proj + 1))
                                                     robots
                                              )
                                , build robot
                                )
                         | otherwise = Nothing
                    in mRobot
                 )
                 [Ore, Clay, Obsidian, Geode]
  where
    cost rob = M.map negate (blueprint M.! rob)
    after rob = M.unionWith (+) resources (cost rob)
    build rob = M.insertWith (+) rob 1 robots
    projected rob robs ress
      = M.foldr max
                0
      . M.mapWithKey (\res reqty ->
                       let (q,r) | get0 res robs > 0
                                 = reqty `quotRem` (get0 res robs)
                                 | otherwise = (100 * reqty, 0)
                           q' | r == 0 = q
                              | otherwise = q + 1
                        in q'
                     )
      $ M.filter (> 0)
                 (M.unionWith (+)
                              (blueprint M.! rob)
                              (M.map negate ress)
                 )

geodes :: Blueprint -> PQ.MinQueue [(Int, Resources, Robots)] -> Int -> Int
geodes blueprint queue bestSeen
  | PQ.null queue = bestSeen
  | minutes == 0, let qty = get0 Geode resources
  = if qty > bestSeen
    then geodes blueprint queue' qty
    else geodes blueprint queue' bestSeen
  | otherwise
  = (\q -> geodes blueprint q bestSeen)
  . foldr (\(minutes', resources', robots') q ->
            PQ.insert ((minutes', resources', robots'):past) q
          )
          queue'
  $ options
  where
    (past@((minutes, resources, robots):_), queue')
      = PQ.deleteFindMin queue

    options = produceRobots blueprint resources robots minutes

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = IM.foldrWithKey (\iD blueprint qualitySum ->
                                 let maxGeodes
                                       = geodes blueprint
                                                (PQ.singleton
                                                  [( 24
                                                  , M.empty
                                                  , M.singleton Ore 1
                                                  )
                                                  ]
                                                )
                                                0
                                  in iD * maxGeodes + qualitySum
                               )
                               0
           <$> input
  printAnswer "Sum of quality levels of blueprints: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = const "P" <$> input
  printAnswer "No answer yet: " answer

main :: IO ()
main = do
  let day = "Day 19: Not Enough Minerals"
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
