module CubeNets where

import Data.List (find)
import Data.Maybe (fromJust)
import qualified Data.Set as S

type Coord = Int
type Coords = (Coord, Coord)
type Direction = Int
type Position = (Coord, Coord, Direction)

north, east, south, west :: Direction
(north, east, south, west) = (3, 0, 1, 2)

turn :: Direction -> Char -> Direction
turn orientation 'L' = (orientation - 1 + 4) `rem` 4
turn orientation 'R' = (orientation + 1) `rem` 4

-- Cube nets:
-- ###  ##  ##  ##  #   #    #   #  ##   #  #
--  #  ##   #   #   ## ### ###  ##  #   ##  #
--  #   #  ##   #  ##   #   #  ##  ##  ##  ##
--  #   #   #  ##   #   #   #   #  #   #   #
--                                         #
-- 1   2     3     4      5       6       7        8          9
-- v | ## | ### | ### | ####  | ># #< |  ###< |  #< >#  |  ##< >##
-- # | ># | v # | # ^ | v  #< |  # #  | ##    | ##   ## | ##     ##
-- # |    |   ^ | v   |       | .# #. | v     | #     # | #       #
-- # | ## | ### | ### |  #### | ## ## | >###  | #     # | v       v
-- # | #< | # v | ^ # | >#  v | v   v |    ## | v     v |
-- v |    | ^   |   v |       |       |     v |         |
--
--   10        =10        11           12
--   v v   |   v v   |    v v    |     v v
--  ## ##  |   # #   |   ## ##   |   ### ###
--  #   #  |  ## ##  |  ##   ##  | ###     ###
-- ##   ## |  #   #  | ##     ## | v         v
-- #     # | ##   ## | v       v |
-- v     v | v     v |           |
--
--  First draft full of errors
--  1: ((y // side - 4) + 1,x)
--  2: (y + (side - x mod side), (x // side + 1) * side + 1)
--     (y + x mod side, (x // side) * side)
--  3: (y + side, (side - x mod side) + (x // side + 2) * side)
--     (y + side, (side - x mod side) + (x // side - 2) * side)
--  4: (y - side, (side - x mod side) + (x // side + 2) * side)
--     (y - side, (side - x mod side) + (x // side - 2) * side)
--  5: (y + x mod side, (x // side + 4) * side)
--     (y + (side - x mod side), (x // side + 1 - 4) * side + 1)
--  6: ( y - (y // side - 4) * side + (side - x mod side)
--     , (x // side + 1) * side + 1
--     )
--     (y - (y // side - 4) * side + (x mod side), (x // side) * side)
--  7: ((y // side - 2) + (side - x mod side), (x // side + 4) * side)
--     ( (y // side - 2) * side + (side - x mod side)
--     , (x // side + 1 - 4) * side + 1
--     )
--  8: ((y // side - 4) * side + (x mod side), (x // side + 2) * side)
--     ( (y // side - 4) * side + (side - x mod side)
--     , (x // side - 1) * side + 1
--     )
--  9: ((y // side - 3) * side + (x mod side), (x // side + 3) * side)
--     ( (y // side - 3) * side + (side - x mod side)
--     , (x // side - 2) * side + 1
--     )
-- 10: ((y // side - 4) * side + 1, x + 2 * side)
--     ((y // side - 4) * side + 1, x - 2 * side)
-- 11: ((y // side - 4) * side + 1, x + 2 * side)
--     ((y // side - 4) * side + 1, x - 2 * side)
-- 12: ((y // side - 2) * side + 1, x + 4 * side)
--     ((y // side - 2) * side + 1, x - 4 * side)

candidate :: S.Set Coords -> Position -> Position
candidate tiles (y,x,o)
  = fromJust
  . find (\(y,x,o) -> (y,x) `S.member` tiles)
  $ candidates
  where
    still, left, right, around :: [Coords] -> [Position]
    still = map (\(y,x) -> (y,x,o))
    left = map (\(y,x) -> (y,x,turn o 'L'))
    right = map (\(y,x) -> (y,x,turn o 'R'))
    around = map (\(y,x) -> (y,x,turn (turn o 'L') 'L'))

    -- Assumption: The square root is always an integer and round trips through
    --             a Floating type.
    side :: Int
    side = round . sqrt . fromIntegral $ S.size tiles `quot` 6

    xForward, xReverse, xSides :: Coord
    xForward = let r = x `rem` side
                   r' | r == 0 = side
                      | otherwise = r
                in r'
    xReverse = 1 + side - xForward
    xSides = x `quot` side - (if xForward == side then 1 else 0)

    yForward, yReverse, ySides :: Coord
    yForward = let r = y `rem` side
                   r' | r == 0 = side
                      | otherwise = r
                in r'
    yReverse = 1 + side - yForward
    ySides = y `quot` side - (if yForward == side then 1 else 0)

    filterBehind :: Int -> Int -> (Bool -> Bool) -> [a] -> [a]
    filterBehind back sideways b2B =
      filter (const (let behind | o == east
                                = map (\op -> ( y `op` (sideways * side)
                                              , x - back * side
                                              )
                                      )
                                      [(-),(+)]
                                | o == south
                                = map (\op -> ( y - back * side
                                              , x `op` (sideways * side)
                                              )
                                      )
                                      [(-),(+)]
                                | o == west
                                = map (\op -> ( y `op` (sideways * side)
                                              , x + back * side
                                              )
                                      )
                                      [(-),(+)]
                                | o == north
                                = map (\op -> ( y + back * side
                                              , x `op` (sideways * side)
                                              )
                                      )
                                      [(-),(+)]
                      in all (b2B . (`S.member` tiles)) behind
                    )
             )

    blockBehind :: [a] -> [a]
    blockBehind = filterBehind 1 0 id

    twoBehind :: [a] -> [a]
    twoBehind = filterBehind 2 0 id

    emptyBehind :: [a] -> [a]
    emptyBehind = filterBehind 1 0 not

    emptySide :: [a] -> [a]
    emptySide = filterBehind 1 4 not

    -- TODO: Check spots that need to be empty.
    -- E 0    S 1    W 2    N 3
    candidates :: [Position]
    candidates
      = map (!! o) $
            -- 1
            [ still [ (y, (xSides - 3) * side + 1)
                    , ((ySides - 3) * side + 1, x)
                    , (y, (xSides + 4) * side)
                    , ((ySides + 4) * side, x)
                    ]
            -- 2
            , left [ (ySides * side, x + yForward)
                   , (y + xReverse, (xSides + 1) * side + 1)
                   , ((ySides + 1) * side + 1, (xSides - 1) * side + yForward)
                   , (y - xForward, xSides * side)
                   ]
            , right [ ((ySides + 1) * side + 1, (xSides + 1) * side + yReverse)
                    , (y + xForward, xSides * side)
                    , (ySides * side, (xSides - 1) * side + yReverse)
                    , (y - xReverse, (xSides + 1) * side + 1)
                    ]
            -- 3
            , around [ ((ySides - 2) * side + yReverse, x + side)
                     , (y + side, (xSides + 2) * side + xReverse)
                     , ((ySides + 2) * side + yReverse, x - side)
                     , (y - side, (xSides - 2) * side + xReverse)
                     ]
            , around [ ((ySides + 2) * side + yReverse, x + side)
                     , (y + side, (xSides - 2) * side + xReverse)
                     , ((ySides - 2) * side + yReverse, x - side)
                     , (y - side, (xSides + 2) * side + xReverse)
                     ]
            ] <>
            -- 4
            (blockBehind
              [ around [ ((ySides - 2) * side + yReverse, x - side)
                       , (y - side, (xSides + 2) * side + xReverse)
                       , ((ySides + 2) * side + yReverse, x + side)
                       , (y + side, (xSides - 2) * side + xReverse)
                       ]
              , around [ ((ySides + 2) * side + yReverse, x - side)
                       , (y - side, (xSides - 2) * side + xReverse)
                       , ((ySides - 2) * side + yReverse, x + side)
                       , (y + side, (xSides + 2) * side + xReverse)
                       ]
              ]
            ) <>
            -- 5
            [ right [ ((ySides - 3) * side + 1, x + yReverse)
                    , (y + xForward, (xSides + 4) * side)
                    , ((ySides + 4) * side, x - yForward)
                    , ((ySides - 1) * side + xForward, (xSides - 3) * side + 1)
                    ]
            , left [ ((ySides + 4) * side, x + yForward)
                   , (y + xReverse, (xSides - 3) * side + 1)
                   , ((ySides - 3) * side + 1, (xSides - 1) * side + yForward)
                   , ((ySides - 1) * side + xReverse, (xSides + 4) * side)
                   ]
            ] <>
            -- 6
            (emptyBehind
              [ left [ (ySides * side, (xSides - 3) * side + yForward)
                     , ((ySides - 3) * side + xReverse, (xSides + 1) * side + 1)
                     , ((ySides + 1) * side + 1, (xSides + 3) * side + yForward)
                     , ((ySides + 3) * side + xReverse, xSides * side)
                     ]
              , right [ ((ySides + 1) * side + 1, (xSides - 3) * side + yReverse)
                      , ((ySides - 3) * side + xForward, xSides * side)
                      , (ySides * side, (xSides + 3) * side + yReverse)
                      , ((ySides + 3) * side + xForward, (xSides + 1) * side + 1)
                      ]
              ]
            ) <>
            -- 7
            (emptySide
            [ right [ ((ySides - 3) * side + 1, (xSides - 1) * side + yReverse)
                    , ((ySides - 1) * side + xForward, (xSides + 4) * side)
                    , ((ySides + 4) * side, (xSides + 1) * side + yReverse)
                    , ((ySides + 1) * side + xForward, (xSides - 3) * side + 1)
                    ]
            , left [ ((ySides + 4) * side, (xSides - 1) * side + yForward)
                   , ((ySides - 1) * side + xReverse, (xSides - 3) * side + 1)
                   , ((ySides - 3) * side + 1, (xSides + 1) * side + yForward)
                   , ((ySides + 1) * side + xReverse, (xSides + 4) * side)
                   ]
            ]
            ) <>
            -- 8
            (twoBehind
            [ right [ ((ySides - 1) * side + 1, (xSides - 3) * side + yReverse)
                    , ((ySides - 3) * side + xForward, (xSides + 2) * side)
                    , ((ySides + 2) * side, (xSides + 3) * side + yReverse)
                    , ((ySides + 3) * side + xForward, (xSides - 1) * side + 1)
                    ]
            , left [ ((ySides + 2) * side, (xSides - 3) * side + yForward)
                   , ((ySides - 3) * side + xReverse, (xSides - 1) * side + 1)
                   , ((ySides - 1) * side + 1, (xSides + 3) * side + yForward)
                   , ((ySides + 3) * side + xReverse, (xSides + 2) * side)
                   ]
            ]
            ) <>
            -- 9
            (blockBehind
            [ right [ ((ySides - 2) * side + 1, (xSides - 2) * side + yReverse)
                    , ((ySides - 2) * side + xForward, (xSides + 3) * side)
                    , ((ySides + 3) * side, (xSides + 2) * side + yReverse)
                    , ((ySides + 2) * side + xForward, (xSides - 2) * side + 1)
                    ]
            , left [ ((ySides + 3) * side, (xSides - 2) * side + yForward)
                   , ((ySides - 2) * side + xReverse, (xSides - 2) * side + 1)
                   , ((ySides - 2) * side + 1, (xSides + 2) * side + yForward)
                   , ((ySides + 2) * side + xReverse, (xSides + 3) * side)
                   ]
            ]
            ) <>
            -- 10
            [ still [ (y - 2 * side, (xSides - 3) * side + 1)
                    , ((ySides - 3) * side + 1, x + 2 * side)
                    , (y + 2 * side, (xSides + 4) * side)
                    , ((ySides + 4) * side, x - 2 * side)
                    ]
            , still [ (y + 2 * side, (xSides - 3) * side + 1)
                    , ((ySides - 3) * side + 1, x - 2 * side)
                    , (y - 2 * side, (xSides + 4) * side)
                    , ((ySides + 4) * side, x + 2 * side)
                    ]
            -- 11
            , still [ ((ySides - 3) * side + yForward, (xSides - 2) * side + 1)
                    , ((ySides - 2) * side + 1, (xSides + 3) * side + xForward)
                    , ((ySides + 3) * side + yForward, (xSides + 3) * side)
                    , ((ySides + 3) * side, (xSides - 3) * side + xForward)
                    ]
            , still [ ((ySides + 3) * side + yForward, (xSides - 2) * side + 1)
                    , ((ySides - 2) * side + 1, (xSides - 3) * side + xForward)
                    , ((ySides - 3) * side + yForward, (xSides + 3) * side)
                    , ((ySides + 3) * side, (xSides + 3) * side + xForward)
                    ]
            -- 12
            , still [ (y - 4 * side, (xSides - 1) * side + 1)
                    , ((ySides - 1) * side + 1, x + 4 * side)
                    , (y + 4 * side, (xSides + 2) * side)
                    , ((ySides + 2) * side, x - 4 * side)
                    ]
            , still [ (y + 4 * side, (xSides - 1) * side + 1)
                    , ((ySides - 1) * side + 1, x - 4 * side)
                    , (y - 4 * side, (xSides + 2) * side)
                    , ((ySides + 2) * side, x + 4 * side)
                    ]
            ]

