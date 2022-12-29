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

import Debug.Trace
import Data.List

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

projected :: Blueprint -> Robot -> Robots -> Resources -> Int
projected blueprint rob robs ress
  = M.foldr max
            0
  . M.mapWithKey (\res reqty ->
                   let (q,r) | get0 res robs > 0
                             = reqty `quotRem` (get0 res robs)
                             | otherwise = (10000 * reqty, 0)
                       q' | r == 0 = q
                          | otherwise = q + 1
                    in q'
                 )
  $ M.filter (> 0)
             (M.unionWith (+)
                          (blueprint M.! rob)
                          (M.map negate ress)
             )

produceRobots :: Blueprint -> Resources -> Robots -> Int -> [Robot]
              -> [(Int, (Resources, Robots,[Robot]))]
produceRobots blueprint resources robots minutes usefulRobots
  | minutes == 0 = error "Can't produce robots without time"
  | minutes == 1
  = [ ( 0
      , ( M.unionWith (+) resources (M.map (* minutes) robots)
        , robots
        , []
        )
      )
    ]
  | otherwise
  = ( 0
    , ( M.unionWith (+) resources (M.map (* minutes) robots)
      , robots
      , []
      )
    ) : ( mapMaybe (\robot ->
                     let proj = projected blueprint robot robots resources
                         mRobot
                           | minutes - proj > thresholds M.! robot
                           = Just ( minutes - proj - 1
                                  , ( M.unionWith (+)
                                                  (after robot)
                                                  (M.map (* (proj + 1)) robots)
                                    , build robot
                                    , usefulRobots'
                                    )
                                  )
                           | otherwise = Nothing
                      in mRobot
                   )
                   usefulRobots'
        )
  where
    cost rob = M.map negate (blueprint M.! rob)
    after rob = M.unionWith (+) resources (cost rob)
    build rob = M.insertWith (+) rob 1 robots
    thresholds = M.fromAscList [(Ore,3), (Clay,5), (Obsidian,3), (Geode,1)]
    geodeCovered = M.foldrWithKey (\req qty ->
                                    ( (get0 req robots
                                      + (get0 req resources
                                        `quot` (minutes - 1)
                                        )
                                      ) >= qty
                                      &&
                                    )
                                  )
                                  True
                                  (blueprint M.! Geode)
    usefulRobots'
      | geodeCovered = [Geode]
      | otherwise
      = filter (\robot ->
                 let mMaxReq = foldr (\r mMax ->
                                       let m' = get0 robot (blueprint M.! r)
                                           mMax' | Nothing <- mMax = Just m'
                                                 | Just m <- mMax
                                                 , m' > m
                                                 = Just m'
                                                 | otherwise = mMax
                                        in mMax'
                                     )
                                     Nothing
                                     usefulRobots
                     enough | robot == Geode = False
                            | Just maxReq <- mMaxReq
                            , get0 robot robots
                            + (get0 robot resources `quot` (minutes - 1))
                            >= maxReq
                            = True
                            | otherwise = False
                  in not enough
               )
               usefulRobots

geodes :: Blueprint -> PQ.MinQueue [(Int, (Resources, Robots, [Robot]))] -> Int
       -> Int
geodes blueprint queue bestSeen
  | PQ.null queue = bestSeen
  | minutes == 0, let qty = get0 Geode resources
  = if qty > bestSeen
    then geodes blueprint queue' qty
    else geodes blueprint queue' bestSeen
  | otherwise
  = geodes blueprint
           (foldr (\v -> PQ.insert (v:past))
                  queue'
                  (produceRobots blueprint
                                 resources
                                 robots
                                 minutes
                                 usefulRobots
                  )
           )
           bestSeen
  where
    (past@((minutes, (resources, robots, usefulRobots)):_), queue')
      = PQ.deleteFindMin queue

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = IM.foldrWithKey (\iD blueprint qualitySum ->
                                 let maxGeodes
                                       = geodes blueprint
                                                (PQ.singleton
                                                  [(24
                                                  ,( M.empty
                                                  , M.singleton Ore 1
                                                  , [Ore, Clay, Obsidian, Geode]
                                                  ))]
                                                )
                                                0
                                  in iD * maxGeodes + qualitySum
                               )
                               0
           <$> input
  printAnswer "Sum of quality levels of blueprints: " answer

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = product
             . IM.map (\blueprint ->
                        geodes blueprint
                               (PQ.singleton
                                 [ (32
                                   ,( M.empty
                                 , M.singleton Ore 1
                                 , [Ore, Clay, Obsidian, Geode]
                                 ))]
                               )
                               0
                      )
             . IM.filterWithKey (\k v -> k <= 3)
           <$> input
  printAnswer "Product of maximum geodes for first three blueprints: " answer

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
