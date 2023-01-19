module Main where

import Test.Tasty
import Test.Tasty.Ingredients.FailFast
import Test.Tasty.HUnit

import qualified Data.Set as S

import CubeNets

type Net = S.Set Coords

main = defaultMainWithIngredients (map failFast defaultIngredients) tests

--  1   2   3   4   5   6   7   8   9  10  11
-- ###  ##  ##  ##  #   #    #   #  ##   #  #
--  #  ##   #   #   ## ### ###  ##  #   ##  #
--  #   #  ##   #  ##   #   #  ##  ##  ##  ##
--  #   #   #  ##   #   #   #   #  #   #   #
--                                         #

-- y,x
net1 :: Net
net1 = S.fromAscList
 [ (1,1), (1,2), (1,3), (1,4), (1,5), (1,6)
 , (2,1), (2,2), (2,3), (2,4), (2,5), (2,6)
 ,               (3,3), (3,4)
 ,               (4,3), (4,4)
 ,               (5,3), (5,4)
 ,               (6,3), (6,4)
 ,               (7,3), (7,4)
 ,               (8,3), (8,4)
 ]

net2 :: Net
net2 = S.fromAscList
 [               (1,3), (1,4), (1,5), (1,6)
 ,               (2,3), (2,4), (2,5), (2,6)
 , (3,1), (3,2), (3,3), (3,4)
 , (4,1), (4,2), (4,3), (4,4)
 ,               (5,3), (5,4)
 ,               (6,3), (6,4)
 ,               (7,3), (7,4)
 ,               (8,3), (8,4)
 ]

net3 :: Net
net3 = S.fromAscList
 [               (1,3), (1,4), (1,5), (1,6)
 ,               (2,3), (2,4), (2,5), (2,6)
 ,               (3,3), (3,4)
 ,               (4,3), (4,4)
 , (5,1), (5,2), (5,3), (5,4)
 , (6,1), (6,2), (6,3), (6,4)
 ,               (7,3), (7,4)
 ,               (8,3), (8,4)
 ]

net4 :: Net
net4 = S.fromAscList
 [               (1,3), (1,4), (1,5), (1,6)
 ,               (2,3), (2,4), (2,5), (2,6)
 ,               (3,3), (3,4)
 ,               (4,3), (4,4)
 ,               (5,3), (5,4)
 ,               (6,3), (6,4)
 , (7,1), (7,2), (7,3), (7,4)
 , (8,1), (8,2), (8,3), (8,4)
 ]

net5 :: Net
net5 = S.fromAscList
 [               (1,3), (1,4)
 ,               (2,3), (2,4)
 ,               (3,3), (3,4), (3,5), (3,6)
 ,               (4,3), (4,4), (4,5), (4,6)
 , (5,1), (5,2), (5,3), (5,4)
 , (6,1), (6,2), (6,3), (6,4)
 ,               (7,3), (7,4)
 ,               (8,3), (8,4)
 ]

net6 :: Net
net6 = S.fromAscList
 [               (1,3), (1,4)
 ,               (2,3), (2,4)
 , (3,1), (3,2), (3,3), (3,4), (3,5), (3,6)
 , (4,1), (4,2), (4,3), (4,4), (4,5), (4,6)
 ,               (5,3), (5,4)
 ,               (6,3), (6,4)
 ,               (7,3), (7,4)
 ,               (8,3), (8,4)
 ]

net7 :: Net
net7 = S.fromAscList
 [                             (1,5), (1,6)
 ,                             (2,5), (2,6)
 , (3,1), (3,2), (3,3), (3,4), (3,5), (3,6)
 , (4,1), (4,2), (4,3), (4,4), (4,5), (4,6)
 ,               (5,3), (5,4)
 ,               (6,3), (6,4)
 ,               (7,3), (7,4)
 ,               (8,3), (8,4)
 ]

net8 :: Net
net8 = S.fromAscList
 [                             (1,5), (1,6)
 ,                             (2,5), (2,6)
 ,               (3,3), (3,4), (3,5), (3,6)
 ,               (4,3), (4,4), (4,5), (4,6)
 , (5,1), (5,2), (5,3), (5,4)
 , (6,1), (6,2), (6,3), (6,4)
 ,               (7,3), (7,4)
 ,               (8,3), (8,4)
 ]

net9 :: Net
net9 = S.fromAscList
 [               (1,3), (1,4), (1,5), (1,6)
 ,               (2,3), (2,4), (2,5), (2,6)
 ,               (3,3), (3,4)
 ,               (4,3), (4,4)
 , (5,1), (5,2), (5,3), (5,4)
 , (6,1), (6,2), (6,3), (6,4)
 , (7,1), (7,2)
 , (8,1), (8,2)
 ]

net10 :: Net
net10 = S.fromAscList
 [                             (1,5), (1,6)
 ,                             (2,5), (2,6)
 ,               (3,3), (3,4), (3,5), (3,6)
 ,               (4,3), (4,4), (4,5), (4,6)
 , (5,1), (5,2), (5,3), (5,4)
 , (6,1), (6,2), (6,3), (6,4)
 , (7,1), (7,2)
 , (8,1), (8,2)
 ]

net11 :: Net
net11 = S.fromAscList
 [                 (1,3), (1,4)
 ,                 (2,3), (2,4)
 ,                 (3,3), (3,4)
 ,                 (4,3), (4,4)
 ,  (5,1),  (5,2), (5,3), (5,4)
 ,  (6,1),  (6,2), (6,3), (6,4)
 ,  (7,1),  (7,2)
 ,  (8,1),  (8,2)
 ,  (9,1),  (9,2)
 , (10,1), (10,2)
 ]

-- Rotations
--           y,x                         x,9-y
--         1,3 1,4 1,5 1,6                  1,5 1,6
--         2,3 2,4 2,5 2,6                  2,5 2,6
-- 3,1 3,2 3,3 3,4          3,1 3,2 3,3 3,4 3,5 3,6 3,7 3,8
-- 4,1 4,2 4,3 4,4          4,1 4,2 4,3 4,4 4,5 4,6 4,7 4,8
--         5,3 5,4                                  5,7 5,8
--         6,3 6,4                                  6,7 6,8
--         7,3 7,4
--         8,3 8,4
--
--         9-y,7-x                       7-x,y
--         1,3 1,4          1,1 1,2
--         2,3 2,4          2,1 2,2
--         3,3 3,4          3,1 3,2 3,3 3,4 3,5 3,6 3,7 3,8
--         4,3 4,4          4,1 4,2 4,3 4,4 4,5 4,6 4,7 4,8
--         5,3 5,4 5,5 5,6          5,3 5,4
--         6,3 6,4 6,5 6,6          6,3 6,4
-- 7,1 7,2 7,3 7,4
-- 8,1 8,2 8,3 8,4
--
--          y,7-x                       7-x,9-y
-- 1,1 1,2 1,3 1,4                                  1,7 1,8
-- 2,1 2,2 2,3 2,4                                  2,7 2,8
--         3,3 3,4 3,5 3,6  3,1 3,2 3,3 3,4 3,5 3,6 3,7 3,8
--         4,3 4,4 4,5 4,6  4,1 4,2 4,3 4,4 4,5 4,6 4,7 4,8
--         5,3 5,4                          5,5 5,6
--         6,3 6,4                          6,5 6,6
--         7,3 7,4
--         8,3 8,4
--
--          9-y,x                         x,y
--         1,3 1,4                  1,3 1,4
--         2,3 2,4                  2,3 2,4
--         3,3 3,4          3,1 3,2 3,3 3,4 3,5 3,6 3,7 3,8
--         4,3 4,4          4,1 4,2 4,3 4,4 4,5 4,6 4,7 4,8
-- 5,1 5,2 5,3 5,4          5,1 5,2
-- 6,1 6,2 6,3 6,4          6,1 6,2
--         7,3 7,4 7,5 7,6
--         8,3 8,4 8,5 8,6

transform :: Coord
          -> Coord
          -> (Coord -> Coord -> Coord -> Coord -> Coord)
          -> (Coord -> Coord -> Coord -> Coord -> Coord)
          -> (Direction -> Direction)
          -> Position
          -> Position
transform yM xM fy fx fo (y,x,o) = (fy yM y xM x, fx yM y xM x, fo o)

tests :: TestTree
tests = testGroup "Tests" $
  map (\(n, (net, ps)) ->
        let yM = 1 + S.findMax (S.map (\(y,_) -> y) net)
            xM = 1 + S.findMax (S.map (\(_,x) -> x) net)
            left = flip turn 'L'
            right = flip turn 'R'
            around = flip turn 'L' . flip turn 'L'
            mirror = ([2,1,0,3] !!)
            tf = transform yM xM
            cY _ y _ _ = y
            cX _ _ _ x = x
            mY yM y _ _ = yM - y
            mX _ _ xM x = xM - x
            change t = ( S.map ((\(y,x,_) -> (y,x)) . t . (\(y,x) -> (y,x,0)))
                               net
                       , map (\(f,g) -> (t f, t g)) ps
                       )
         in testGroup ("Net " <> show n) $
              map (\(yx, (rnet, rps)) ->
                    testGroup yx $
                      map (\(i, (f,t))->
                            testCase (show i <> " - " <> show f)
                                     (t @=? candidate rnet f)
                          )
                          (zip [1..] rps)

                  )
                  [ ("y,x", change (tf cY cX id))
                  , ("x,yM-y", change (tf cX mY right))
                  , ("yM-y,xM-x", change (tf mY mX around))
                  , ("xM-x,y", change (tf mX cY left))
                  , ("y,xM-x", change (tf cY mX mirror))
                  , ("xM-x,yM-y", change (tf mX mY (right . mirror)))
                  , ("yM-y,x", change (tf mY cX (around . mirror)))
                  , ("x,y", change (tf cX cY (left . mirror)))
                  ]
      )
  . zip [1..11]
  $ [ (net1, [ ((1,1,north),(7,3,east))
             , ((1,2,north),(8,3,east))
             , ((1,3,north),(8,3,north))
             , ((1,4,north),(8,4,north))
             , ((1,5,north),(8,4,west))
             , ((1,6,north),(7,4,west))

             , ((1,6,east),(6,4,west))
             , ((2,6,east),(5,4,west))

             , ((2,6,south),(4,4,west))
             , ((2,5,south),(3,4,west))

             , ((3,4,east),(2,5,north))
             , ((4,4,east),(2,6,north))
             , ((5,4,east),(2,6,west))
             , ((6,4,east),(1,6,west))
             , ((7,4,east),(1,6,south))
             , ((8,4,east),(1,5,south))

             , ((8,4,south),(1,4,south))
             , ((8,3,south),(1,3,south))

             , ((8,3,west),(1,2,south))
             , ((7,3,west),(1,1,south))
             , ((6,3,west),(1,1,east))
             , ((5,3,west),(2,1,east))
             , ((4,3,west),(2,1,north))
             , ((3,3,west),(2,2,north))

             , ((2,2,south),(3,3,east))
             , ((2,1,south),(4,3,east))

             , ((2,1,west),(5,3,east))
             , ((1,1,west),(6,3,east))
             ]
      )

    , (net2, [ ((3,1,north),(1,3,east))
             , ((3,2,north),(2,3,east))

             , ((2,3,west),(3,2,south))
             , ((1,3,west),(3,1,south))

             , ((1,3,north),(8,3,north))
             , ((1,4,north),(8,4,north))
             , ((1,5,north),(8,4,west))
             , ((1,6,north),(7,4,west))

             , ((1,6,east),(6,4,west))
             , ((2,6,east),(5,4,west))

             , ((2,6,south),(4,4,west))
             , ((2,5,south),(3,4,west))

             , ((3,4,east),(2,5,north))
             , ((4,4,east),(2,6,north))
             , ((5,4,east),(2,6,west))
             , ((6,4,east),(1,6,west))
             , ((7,4,east),(1,6,south))
             , ((8,4,east),(1,5,south))

             , ((8,4,south),(1,4,south))
             , ((8,3,south),(1,3,south))

             , ((8,3,west),(3,1,east))
             , ((7,3,west),(4,1,east))
             , ((6,3,west),(4,1,north))
             , ((5,3,west),(4,2,north))

             , ((4,2,south),(5,3,east))
             , ((4,1,south),(6,3,east))

             , ((4,1,west),(7,3,east))
             , ((3,1,west),(8,3,east))
             ]
      )

    , (net3, [ ((5,1,north),(3,3,east))
             , ((5,2,north),(4,3,east))

             , ((4,3,west),(5,2,south))
             , ((3,3,west),(5,1,south))

             , ((2,3,west),(5,1,east))
             , ((1,3,west),(6,1,east))

             , ((1,3,north),(8,3,north))
             , ((1,4,north),(8,4,north))
             , ((1,5,north),(8,4,west))
             , ((1,6,north),(7,4,west))

             , ((1,6,east),(6,4,west))
             , ((2,6,east),(5,4,west))

             , ((2,6,south),(4,4,west))
             , ((2,5,south),(3,4,west))

             , ((3,4,east),(2,5,north))
             , ((4,4,east),(2,6,north))
             , ((5,4,east),(2,6,west))
             , ((6,4,east),(1,6,west))
             , ((7,4,east),(1,6,south))
             , ((8,4,east),(1,5,south))

             , ((8,4,south),(1,4,south))
             , ((8,3,south),(1,3,south))

             , ((8,3,west),(6,1,north))
             , ((7,3,west),(6,2,north))

             , ((6,2,south),(7,3,east))
             , ((6,1,south),(8,3,east))

             , ((6,1,west),(1,3,east))
             , ((5,1,west),(2,3,east))
             ]
      )

    , (net4, [ ((7,1,north),(5,3,east))
             , ((7,2,north),(6,3,east))

             , ((6,3,west),(7,2,south))
             , ((5,3,west),(7,1,south))
             , ((4,3,west),(7,1,east))
             , ((3,3,west),(8,1,east))
             , ((2,3,west),(8,1,north))
             , ((1,3,west),(8,2,north))

             , ((1,3,north),(8,3,north))
             , ((1,4,north),(8,4,north))
             , ((1,5,north),(8,4,west))
             , ((1,6,north),(7,4,west))

             , ((1,6,east),(6,4,west))
             , ((2,6,east),(5,4,west))

             , ((2,6,south),(4,4,west))
             , ((2,5,south),(3,4,west))

             , ((3,4,east),(2,5,north))
             , ((4,4,east),(2,6,north))
             , ((5,4,east),(2,6,west))
             , ((6,4,east),(1,6,west))
             , ((7,4,east),(1,6,south))
             , ((8,4,east),(1,5,south))

             , ((8,4,south),(1,4,south))
             , ((8,3,south),(1,3,south))
             , ((8,2,south),(1,3,east))
             , ((8,1,south),(2,3,east))

             , ((8,1,west),(3,3,east))
             , ((7,1,west),(4,3,east))
             ]
      )

    , (net5, [ ((5,1,north),(3,3,east))
             , ((5,2,north),(4,3,east))

             , ((4,3,west),(5,2,south))
             , ((3,3,west),(5,1,south))

             , ((2,3,west),(5,1,east))
             , ((1,3,west),(6,1,east))

             , ((1,3,north),(8,3,north))
             , ((1,4,north),(8,4,north))

             , ((1,4,east),(3,6,south))
             , ((2,4,east),(3,5,south))

             , ((3,6,north),(1,4,west))
             , ((3,5,north),(2,4,west))

             , ((3,6,east),(8,4,west))
             , ((4,6,east),(7,4,west))

             , ((4,6,south),(6,4,west))
             , ((4,5,south),(5,4,west))

             , ((5,4,east),(4,5,north))
             , ((6,4,east),(4,6,north))
             , ((7,4,east),(4,6,west))
             , ((8,4,east),(3,6,west))

             , ((8,4,south),(1,4,south))
             , ((8,3,south),(1,3,south))

             , ((8,3,west),(6,1,north))
             , ((7,3,west),(6,2,north))

             , ((6,2,south),(7,3,east))
             , ((6,1,south),(8,3,east))

             , ((6,1,west),(1,3,east))
             , ((5,1,west),(2,3,east))
             ]
      )

    , (net6, [ ((3,1,north),(1,3,east))
             , ((3,2,north),(2,3,east))

             , ((2,3,west),(3,2,south))
             , ((1,3,west),(3,1,south))

             , ((1,3,north),(8,3,north))
             , ((1,4,north),(8,4,north))

             , ((1,4,east),(3,6,south))
             , ((2,4,east),(3,5,south))

             , ((3,6,north),(1,4,west))
             , ((3,5,north),(2,4,west))

             , ((3,6,east),(8,4,west))
             , ((4,6,east),(7,4,west))

             , ((4,6,south),(6,4,west))
             , ((4,5,south),(5,4,west))

             , ((5,4,east),(4,5,north))
             , ((6,4,east),(4,6,north))
             , ((7,4,east),(4,6,west))
             , ((8,4,east),(3,6,west))

             , ((8,4,south),(1,4,south))
             , ((8,3,south),(1,3,south))

             , ((8,3,west),(3,1,east))
             , ((7,3,west),(4,1,east))
             , ((6,3,west),(4,1,north))
             , ((5,3,west),(4,2,north))

             , ((4,2,south),(5,3,east))
             , ((4,1,south),(6,3,east))

             , ((4,1,west),(7,3,east))
             , ((3,1,west),(8,3,east))
             ]
      )

    , (net7, [ ((3,1,north),(1,6,south))
             , ((3,2,north),(1,5,south))
             , ((3,3,north),(1,5,east))
             , ((3,4,north),(2,5,east))

             , ((2,5,west),(3,4,south))
             , ((1,5,west),(3,3,south))

             , ((1,5,north),(3,2,south))
             , ((1,6,north),(3,1,south))

             , ((1,6,east),(8,3,north))
             , ((2,6,east),(8,4,north))
             , ((3,6,east),(8,4,west))
             , ((4,6,east),(7,4,west))

             , ((4,6,south),(6,4,west))
             , ((4,5,south),(5,4,west))

             , ((5,4,east),(4,5,north))
             , ((6,4,east),(4,6,north))
             , ((7,4,east),(4,6,west))
             , ((8,4,east),(3,6,west))

             , ((8,4,south),(2,6,west))
             , ((8,3,south),(1,6,west))

             , ((8,3,west),(3,1,east))
             , ((7,3,west),(4,1,east))
             , ((6,3,west),(4,1,north))
             , ((5,3,west),(4,2,north))

             , ((4,2,south),(5,3,east))
             , ((4,1,south),(6,3,east))

             , ((4,1,west),(7,3,east))
             , ((3,1,west),(8,3,east))
             ]
      )

    , (net8, [ ((5,1,north),(3,3,east))
             , ((5,2,north),(4,3,east))

             , ((4,3,west),(5,2,south))
             , ((3,3,west),(5,1,south))

             , ((3,3,north),(1,5,east))
             , ((3,4,north),(2,5,east))

             , ((2,5,west),(3,4,south))
             , ((1,5,west),(3,3,south))

             , ((1,5,north),(5,1,east))
             , ((1,6,north),(6,1,east))

             , ((1,6,east),(8,3,north))
             , ((2,6,east),(8,4,north))
             , ((3,6,east),(8,4,west))
             , ((4,6,east),(7,4,west))

             , ((4,6,south),(6,4,west))
             , ((4,5,south),(5,4,west))

             , ((5,4,east),(4,5,north))
             , ((6,4,east),(4,6,north))
             , ((7,4,east),(4,6,west))
             , ((8,4,east),(3,6,west))

             , ((8,4,south),(2,6,west))
             , ((8,3,south),(1,6,west))

             , ((8,3,west),(6,1,north))
             , ((7,3,west),(6,2,north))

             , ((6,2,south),(7,3,east))
             , ((6,1,south),(8,3,east))

             , ((6,1,west),(1,6,south))
             , ((5,1,west),(1,5,south))
             ]
      )

    , (net9, [ ((5,1,north),(3,3,east))
             , ((5,2,north),(4,3,east))

             , ((4,3,west),(5,2,south))
             , ((3,3,west),(5,1,south))
             , ((2,3,west),(5,1,east))
             , ((1,3,west),(6,1,east))

             , ((1,3,north),(7,1,east))
             , ((1,4,north),(8,1,east))
             , ((1,5,north),(8,1,north))
             , ((1,6,north),(8,2,north))

             , ((1,6,east),(6,4,west))
             , ((2,6,east),(5,4,west))

             , ((2,6,south),(4,4,west))
             , ((2,5,south),(3,4,west))

             , ((3,4,east),(2,5,north))
             , ((4,4,east),(2,6,north))
             , ((5,4,east),(2,6,west))
             , ((6,4,east),(1,6,west))

             , ((6,4,south),(8,2,west))
             , ((6,3,south),(7,2,west))

             , ((7,2,east),(6,3,north))
             , ((8,2,east),(6,4,north))

             , ((8,2,south),(1,6,south))
             , ((8,1,south),(1,5,south))

             , ((8,1,west),(1,4,south))
             , ((7,1,west),(1,3,south))
             , ((6,1,west),(1,3,east))
             , ((5,1,west),(2,3,east))
             ]
      )

    , (net10, [ ((5,1,north),(3,3,east))
              , ((5,2,north),(4,3,east))

              , ((4,3,west),(5,2,south))
              , ((3,3,west),(5,1,south))

              , ((3,3,north),(1,5,east))
              , ((3,4,north),(2,5,east))

              , ((2,5,west),(3,4,south))
              , ((1,5,west),(3,3,south))

              , ((1,5,north),(5,1,east))
              , ((1,6,north),(6,1,east))

              , ((1,6,east),(7,1,east))
              , ((2,6,east),(8,1,east))
              , ((3,6,east),(8,1,north))
              , ((4,6,east),(8,2,north))

              , ((4,6,south),(6,4,west))
              , ((4,5,south),(5,4,west))

              , ((5,4,east),(4,5,north))
              , ((6,4,east),(4,6,north))

              , ((6,4,south),(8,2,west))
              , ((6,3,south),(7,2,west))

              , ((7,2,east),(6,3,north))
              , ((8,2,east),(6,4,north))

              , ((8,2,south),(4,6,west))
              , ((8,1,south),(3,6,west))

              , ((8,1,west),(2,6,west))
              , ((7,1,west),(1,6,west))
              , ((6,1,west),(1,6,south))
              , ((5,1,west),(1,5,south))
              ]
      )

    , (net11, [ ((5,1,north),(3,3,east))
              , ((5,2,north),(4,3,east))

              , ((4,3,west),(5,2,south))
              , ((3,3,west),(5,1,south))
              , ((2,3,west),(5,1,east))
              , ((1,3,west),(6,1,east))

              , ((1,3,north),(7,1,east))
              , ((1,4,north),(8,1,east))

              , ((1,4,east),(9,1,east))
              , ((2,4,east),(10,1,east))
              , ((3,4,east),(10,1,north))
              , ((4,4,east),(10,2,north))
              , ((5,4,east),(10,2,west))
              , ((6,4,east),(9,2,west))

              , ((6,4,south),(8,2,west))
              , ((6,3,south),(7,2,west))

              , ((8,2,east),(6,4,north))
              , ((7,2,east),(6,3,north))
              , ((9,2,east),(6,4,west))
              , ((10,2,east),(5,4,west))

              , ((10,2,south),(4,4,west))
              , ((10,1,south),(3,4,west))

              , ((5,1,west),(2,3,east))
              , ((6,1,west),(1,3,east))
              , ((7,1,west),(1,3,south))
              , ((8,1,west),(1,4,south))
              , ((9,1,west),(1,4,west))
              , ((10,1,west),(2,4,west))
              ]
      )

-- [                 (1,3), (1,4)
-- ,                 (2,3), (2,4)
-- ,                 (3,3), (3,4)
-- ,                 (4,3), (4,4)
-- ,  (5,1),  (5,2), (5,3), (5,4)
-- ,  (6,1),  (6,2), (6,3), (6,4)
-- ,  (7,1),  (7,2)
-- ,  (8,1),  (8,2)
-- ,  (9,1),  (9,2)
-- , (10,1), (10,2)
-- ]
    ]
