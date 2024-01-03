module Main where

import Criterion.Main
import System.IO.Silently (silence)
import Text.Megaparsec
import Text.Megaparsec.Char

import AoC

import qualified Data.Map as M
import qualified Data.PQueue.Prio.Max as PQ
import qualified Data.Set as S

type Gardens = M.Map YX Char

type Input = [[Char]]

parser :: Parser Input
parser = characterGrid <* eof

findStart :: Gardens -> YX
findStart = M.foldrWithKey (\yx p next -> if p == 'S' then yx else next)
                           (error "No starting position found!")

neighbors :: Gardens -> YX -> S.Set YX
neighbors gardens (y,x) =
    S.fromAscList
  . filter (\p -> M.findWithDefault '#' p gardens /= '#')
  $ [(y + dy, x + dx) | dy <- [-1..1]
                      , dx <- [-1..1]
                      , abs dy /= abs dx
    ]

reach :: Int -> Gardens -> Int
reach steps gardens = length
                    . snd
                    $ go (PQ.singleton steps (findStart gardens))
                         (S.singleton (findStart gardens), mempty)
  where
    go candidates (seen,reachable)
      | null candidates = (seen, reachable)
      | ((steps, yx), rest) <- PQ.deleteFindMax candidates
      = let reachable' | even steps = S.insert yx reachable
                       | otherwise = reachable
            ns = S.filter (`S.notMember` seen) (neighbors gardens yx)
            candidates' | steps == 0 = rest
                        | otherwise = S.foldr (PQ.insert (steps - 1)) rest ns
         in go candidates' (seen <> ns, reachable')

part1 :: Parsed Input -> IO ()
part1 input = do
  let answer = reach 64 . foldYX <$> input
  printAnswer "Reachable plots in 64 steps: " answer

-- Two helper functions to simulate on small (n * 131 + 65) step inputs to find
-- all possible configurations.
expandMap :: Int -> Gardens -> Gardens
expandMap dim gardens = M.unions gs
  where
    gardensWithoutStart = M.insert (findStart gardens) '.' gardens
    shiftGarden dy dx = M.mapKeysMonotonic (\(y,x) -> (y + dy, x + dx))
    gs = [ shiftGarden (131 * dy) (131 * dx) g
         | dy <- [-dim..dim]
         , dx <- [-dim..dim]
         , let g = if dy == 0 && dx == 0 then gardens else gardensWithoutStart
         ]

reachPerMap :: Int -> Gardens -> [[Int]]
reachPerMap steps gardens =
    (\gs ->
      foldr (\dy more rs ->
              let (rs',rs'') = S.split ((dy + 1) * 131, -dim * 131) rs
               in foldr (\dx more rs ->
                          let (rs', rs'') = S.partition ((< (dx + 1) * 131) . snd) rs
                           in length rs' : more rs''
                        )
                        (const [])
                        [-dim..dim]
                        rs'
                  : more rs''
            )
            (const [])
            [-dim..dim]
            gs
    )
  . snd
  $ go (PQ.singleton steps (findStart gardens))
       (S.singleton (findStart gardens), mempty)
  where
    dim = steps `quot` 131
    go candidates (seen,reachable)
      | null candidates = (seen, reachable)
      | ((steps, yx), rest) <- PQ.deleteFindMax candidates
      = let reachable' | even steps = S.insert yx reachable
                       | otherwise = reachable
            ns = S.filter (`S.notMember` seen) (neighbors gardens yx)
            candidates' | steps == 0 = rest
                        | otherwise = S.foldr (PQ.insert (steps - 1)) rest ns
         in go candidates' (seen <> ns, reachable')

reachTailored :: Int -> Gardens -> Int
reachTailored steps gardens
  = evenMaps * evenCount + oddMaps * oddCount
  + nCount + eCount + sCount + wCount
  + edgeMaps * (neCount + seCount + swCount + nwCount)
  + furthestReach * (ne2Count + se2Count + sw2Count + nw2Count)
  where
    side = 1 + (fst . fst . M.findMax $ gardens)
    -- This simple division gives the maps we can completely explore because
    -- the Side is enough steps to fully explore a map from the center, one map
    -- back from the edges can thus be fully explored. The center lines are
    -- completely free of obstacles, as are the edges.
    (furthestReach, edgeSteps) = steps `quotRem` side
    -- A lozenge is like two squares made out of the quadrants, one has sides
    -- one shorter than the other.
    --      #
    --    # ##
    --   ## ###
    --  ### ####
    -- #### #####
    --
    --  #### ###
    --   ### ##
    --    ## #
    --     #
    -- Circumference of a lozenge from the radius, half the diagonal.
    circumference :: Int -> Int
    circumference 1 = 1
    circumference n = 4 * (n - 1)
    evenMaps = sum . map (circumference . (1 +)) $ [0,2..furthestReach - 1]
    oddMaps = sum . map (circumference . (1 +)) $ [1,3..furthestReach - 1]
    edgeMaps = furthestReach - 1
    -- Side - 1 is the distance to the furthest corners in a map, this only
    -- works because there's no complicated structure requiring moving
    -- backwards. A spiral would require more moves.
    -- This means all the internal maps are fully explorable and it simply
    -- depends on whether there's an even or odd number of steps left to go.
    evenCount = reach steps gardens
    oddCount = reach (steps - side) gardens
    gardensWithoutStart = M.insert (findStart gardens) '.' gardens
    stepsFromEdgeCenter = edgeSteps + side `quot` 2
    edgeCenter = side `quot` 2
    nCount = reach stepsFromEdgeCenter
                   (M.insert (side - 1, edgeCenter) 'S' gardensWithoutStart)
    eCount = reach stepsFromEdgeCenter
                   (M.insert (edgeCenter, 0) 'S' gardensWithoutStart)
    sCount = reach stepsFromEdgeCenter
                   (M.insert (0, edgeCenter) 'S' gardensWithoutStart)
    wCount = reach stepsFromEdgeCenter
                   (M.insert (edgeCenter, side - 1) 'S' gardensWithoutStart)
    stepsFromCorner = edgeSteps + side - 1
    neCount = reach stepsFromCorner
                    (M.insert (side - 1, 0) 'S' gardensWithoutStart)
    seCount = reach stepsFromCorner
                    (M.insert (0, 0) 'S' gardensWithoutStart)
    swCount = reach stepsFromCorner
                    (M.insert (0, side - 1) 'S' gardensWithoutStart)
    nwCount = reach stepsFromCorner
                    (M.insert (side - 1, side - 1) 'S' gardensWithoutStart)
    -- You can in fact reach into the near corners of the maps in between the
    -- edge maps. The lozenge edge is a line of plots, not maps.
    stepsFromCorner2 = edgeSteps - 1
    ne2Count = reach stepsFromCorner2
                     (M.insert (side - 1, 0) 'S' gardensWithoutStart)
    se2Count = reach stepsFromCorner2
                     (M.insert (0, 0) 'S' gardensWithoutStart)
    sw2Count = reach stepsFromCorner2
                     (M.insert (0, side - 1) 'S' gardensWithoutStart)
    nw2Count = reach stepsFromCorner2
                     (M.insert (side - 1, side - 1) 'S' gardensWithoutStart)

part2 :: Parsed Input -> IO ()
part2 input = do
  let answer = reachTailored 26501365 . foldYX <$> input
  printAnswer "Reachable plots in 26501365 steps: " answer

main :: IO ()
main = do
  let day = "Day 21: Step Counter"
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
