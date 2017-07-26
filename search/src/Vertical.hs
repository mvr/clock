module Vertical where

import Data.SBV

type Ring = [SBool]

isRepresenting :: (SBool, SBool, SBool, SBool) -> Ring -> Ring -> SBool
isRepresenting (l1, l2, r1, r2) ring1 ring2
  =   (((ring1 !! 0) ||| (ring2 !! 0))           .== false)
  &&& (((ring1 !! 1) ||| (ring2 !! 1))           .== l1)
  &&& (((ring1 !! 2) ||| (ring2 !! 2))           .== l2)
  &&& (((ring1 !! 3) ||| (ring2 !! 3))           .== false)
  &&& (((ring1 !! h1) ||| (ring2 !! h1))         .== false)
  &&& (((ring1 !! h2) ||| (ring2 !! h2))         .== r2)
  &&& (((ring1 !! h3) ||| (ring2 !! h3))         .== r1)
  &&& (((ring1 !! h4) ||| (ring2 !! h4))         .== false)
  where halfway = length ring1 `quot` 2
        h1 = halfway
        h2 = halfway + 1
        h3 = halfway + 2
        h4 = halfway + 3

-- isRepresenting' (l1, l2, r1, r2) ring1 ring2
--   =   (((ring1 !! 0) ||| (ring2 !! 0))           == l1)
--   &&& (((ring1 !! 1) ||| (ring2 !! 1))           == l2)
--   &&& (((ring1 !! h1) ||| (ring2 !! h1))         == r2)
--   &&& (((ring1 !! h2) ||| (ring2 !! h2))         == r1)
--   where halfway = length ring1 `quot` 2
--         h1 = halfway
--         h2 = halfway + 1

rotateRing :: Int -> [a] -> [a]
rotateRing _ [] = []
rotateRing n xs = zipWith const (drop n (cycle xs)) xs

canRepresent :: (SBool, SBool, SBool, SBool) -> Ring -> Ring -> SBool
canRepresent lr ring1 ring2
  = bAny (\(i, j) -> isRepresenting lr (rotateRing i ring1) (rotateRing j ring2)) rotations
  where rotations = [(i, j) | i <- [0 .. length ring1 - 1], j <- [0 .. length ring2 - 1]]

ringsWork :: Ring -> Ring -> SBool
ringsWork ring1 ring2 = bAll (\lr -> canRepresent lr ring1 ring2) $ do
  l1 <- [true, false]
  l2 <- [true, false]
  r1 <- [true, false]
  r2 <- [true, false]
  return (l1, l2, r1, r2)

countTrues :: [SBool] -> SWord8
countTrues [] = 0
countTrues (b:bs) = ite b (1 + r) r
  where r = countTrues bs

findRing n = allSat $ do
  ring1 <- mkExistVars n
  ring2 <- mkExistVars n
  constrain $ countTrues ring1 .>= 4
  constrain $ countTrues ring2 .>= 4
  return (ringsWork ring1 ring2)

allPairs :: Boolean a => [(a, a, a, a)]
allPairs = do
  l1 <- [true, false]
  l2 <- [true, false]
  r1 <- [true, false]
  r2 <- [true, false]
  return (l1, l2, r1, r2)

-- findPosFor ring1 ring2 lr = do
--   i <- [0 .. length ring1 - 1]
--   j <- [0 .. length ring2 - 1]

--   if isRepresenting' lr (rotateRing i ring1) (rotateRing j ring2) then
--     [(i, j)]
--   else
--     []

test1 = [False, True, False, False, True, True, False, False, False, False, False, False, True, False]
test2 = [False, False, False, False, True, False, True, False, False, False, False, False, True, True]
lookup = [((True,True,True,True),[(1,5),(4,5),(4,6),(4,11),(4,13),(5,5),(5,12),(8,12),(11,4),(11,6),(11,12),(11,13),(12,5),(12,12)]),((True,True,True,False),[(0,4),(0,12),(1,12),(2,12),(3,4),(3,12),(4,0),(4,1),(4,2),(4,3),(4,4),(4,7),(4,8),(4,9),(4,10),(4,12),(6,12),(7,12),(9,12),(10,12),(13,12)]),((True,True,False,True),[(0,6),(0,13),(1,11),(3,6),(3,13),(5,3),(5,11),(12,3),(12,11)]),((True,True,False,False),[(1,3)]),((True,False,True,True),[(5,4),(5,10),(7,6),(7,13),(8,4),(10,6),(10,13),(12,4),(12,10)]),((True,False,True,False),[(1,4),(1,10),(2,4),(6,4),(7,4),(9,4),(10,4),(13,4)]),((True,False,False,True),[(1,6),(1,13),(2,6),(2,13),(5,0),(5,1),(5,2),(5,6),(5,7),(5,8),(5,9),(5,13),(6,6),(6,13),(8,6),(8,13),(9,6),(9,13),(12,0),(12,1),(12,2),(12,6),(12,7),(12,8),(12,9),(12,13),(13,6),(13,13)]),((True,False,False,False),[(1,0),(1,1),(1,2),(1,7),(1,8),(1,9)]),((False,True,True,True),[(0,5),(2,5),(3,5),(6,5),(7,5),(7,11),(8,5),(9,5),(10,5),(10,11),(11,0),(11,1),(11,2),(11,3),(11,5),(11,7),(11,8),(11,9),(11,10),(11,11),(13,5)]),((False,True,True,False),[(0,10),(3,10),(7,3),(10,3)]),((False,True,False,True),[(0,11),(2,11),(3,11),(6,11),(8,3),(8,11),(9,11),(13,11)]),((False,True,False,False),[(0,0),(0,1),(0,2),(0,3),(0,7),(0,8),(0,9),(2,3),(3,0),(3,1),(3,2),(3,3),(3,7),(3,8),(3,9),(6,3),(9,3),(13,3)]),((False,False,True,True),[(8,10)]),((False,False,True,False),[(2,10),(6,10),(7,0),(7,1),(7,2),(7,7),(7,8),(7,9),(7,10),(9,10),(10,0),(10,1),(10,2),(10,7),(10,8),(10,9),(10,10),(13,10)]),((False,False,False,True),[(8,0),(8,1),(8,2),(8,7),(8,8),(8,9)]),((False,False,False,False),[(2,0),(2,1),(2,2),(2,7),(2,8),(2,9),(6,0),(6,1),(6,2),(6,7),(6,8),(6,9),(9,0),(9,1),(9,2),(9,7),(9,8),(9,9),(13,0),(13,1),(13,2),(13,7),(13,8),(13,9)])]
