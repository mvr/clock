module Horizontal where

import Data.SBV

type Ring = [SBool]

isRepresenting :: (SBool, SBool, SBool, SBool, SBool, SBool) -> Ring -> Ring -> SBool
isRepresenting (l1, l2, l3, r1, r2, r3) ring1 ring2
  =   (((ring1 !! 0) ||| (ring2 !! 0))           .== l1)
  &&& (((ring1 !! 1) ||| (ring2 !! 1))           .== l2)
  &&& (((ring1 !! 2) ||| (ring2 !! 2))           .== l3)
  &&& (((ring1 !! h1) ||| (ring2 !! h1))         .== r3)
  &&& (((ring1 !! h2) ||| (ring2 !! h2))         .== r2)
  &&& (((ring1 !! h3) ||| (ring2 !! h3))         .== r1)
  where halfway = length ring1 `quot` 2
        h1 = halfway
        h2 = halfway + 1
        h3 = halfway + 2

isRepresenting' (l1, l2, l3, r1, r2, r3) ring1 ring2
  =   (((ring1 !! 0) ||| (ring2 !! 0))           == l1)
  &&& (((ring1 !! 1) ||| (ring2 !! 1))           == l2)
  &&& (((ring1 !! 2) ||| (ring2 !! 2))           == l3)
  &&& (((ring1 !! h1) ||| (ring2 !! h1))         == r3)
  &&& (((ring1 !! h2) ||| (ring2 !! h2))         == r2)
  &&& (((ring1 !! h3) ||| (ring2 !! h3))         == r1)
  where halfway = length ring1 `quot` 2
        h1 = halfway
        h2 = halfway + 1
        h3 = halfway + 2

rotateRing :: Int -> [a] -> [a]
rotateRing _ [] = []
rotateRing n xs = zipWith const (drop n (cycle xs)) xs

canRepresent :: (SBool, SBool, SBool, SBool, SBool, SBool) -> Ring -> Ring -> SBool
canRepresent lr ring1 ring2
  = bAny (\(i, j) -> isRepresenting lr (rotateRing i ring1) (rotateRing j ring2)) rotations
  where rotations = [(i, j) | i <- [0 .. length ring1 - 1], j <- [0 .. length ring2 - 1]]
allTriples :: Boolean a => [(a, a, a, a, a, a)]
allTriples = do
  l1 <- [true, false]
  l2 <- [true, false]
  l3 <- [true, false]
  r1 <- [true, false]
  r2 <- [true, false]
  r3 <- [true, false]
  return (l1, l2, l3, r1, r2, r3)

ringsWork :: Ring -> Ring -> SBool
ringsWork ring1 ring2 = bAll (\lr -> canRepresent lr ring1 ring2) allTriples

findRing n = allSat $ do
  ring1 <- mkExistVars n
  ring2 <- mkExistVars n
  return (ringsWork ring1 ring2)

findPosFor ring1 ring2 lr = do
  i <- [0 .. length ring1 - 1]
  j <- [0 .. length ring2 - 1]

  if isRepresenting' lr (rotateRing i ring1) (rotateRing j ring2) then
    [(i, j)]
  else
    []

test1 = [False, False, False, False, False, True, True, False, False, False, True, False, False, True]
test2 = [False, False, True, True, False, True, False, False, False, True, False, False, False, False]
lookup = [((True,True,True,True,True,True),[(4,9),(11,2)]),
          ((True,True,True,True,True,False),[(5,0),(5,7),(10,1)]),
          ((True,True,True,True,False,True),[(4,2)]),
          ((True,True,True,True,False,False),[(4,3),(4,5)]),
          ((True,True,True,False,True,True),[(6,1),(12,3),(13,1)]),
          ((True,True,True,False,True,False),[(5,1),(5,3)]),
          ((True,True,True,False,False,True),[(3,2),(8,2)]),
          ((True,True,True,False,False,False),[(9,3)]),
          ((True,True,False,True,True,True),[(5,10),(6,8),(13,8)]),
          ((True,True,False,True,True,False),[(5,8),(10,8)]),
          ((True,True,False,True,False,True),[(1,2),(10,2)]),
          ((True,True,False,True,False,False),[(10,4)]),
          ((True,True,False,False,True,True),[(2,2),(5,2),(5,9),(5,12),(9,9),(12,2),(12,5),(12,9)]),
          ((True,True,False,False,True,False),[(5,4),(5,5),(5,6),(5,11),(5,13)]),
          ((True,True,False,False,False,True),[(0,2),(6,2),(6,4),(7,2),(9,2),(13,2),(13,4)]),
          ((True,True,False,False,False,False),[(9,5)]),
          ((True,False,True,True,True,True),[(11,9)]),
          ((True,False,True,True,True,False),[(11,3),(11,5)]),
          ((True,False,True,True,False,True),[(6,0),(6,7),(13,0),(13,7)]),
          ((True,False,True,True,False,False),[(1,3),(10,0),(10,3),(10,7)]),
          ((True,False,True,False,True,True),[(3,9),(8,9)]),
          ((True,False,True,False,True,False),[(2,3)]),
          ((True,False,True,False,False,True),[(3,3),(3,5),(6,3),(13,3)]),
          ((True,False,True,False,False,False),[(0,3),(7,3),(8,3),(8,5)]),
          ((True,False,False,True,True,True),[(1,9),(10,9)]),
          ((True,False,False,True,True,False),[(10,11)]),
          ((True,False,False,True,False,True),[(6,10),(10,10),(10,12),(13,10)]),
          ((True,False,False,True,False,False),[(1,5),(10,5),(10,6),(10,13)]),
          ((True,False,False,False,True,True),[(0,9),(2,9),(6,9),(6,11),(7,9),(13,9),(13,11)]),
          ((True,False,False,False,True,False),[(2,5)]),
          ((True,False,False,False,False,True),[(6,5),(6,6),(6,12),(6,13),(13,5),(13,6),(13,12),(13,13)]),
          ((True,False,False,False,False,False),[(0,5),(7,5)]),
          ((False,True,True,True,True,True),[(3,8),(12,0),(12,7)]),
          ((False,True,True,True,True,False),[(1,1),(4,1),(4,8),(4,11),(8,8),(11,1),(11,4),(11,8)]),
          ((False,True,True,True,False,True),[(4,10),(4,12)]),
          ((False,True,True,True,False,False),[(4,0),(4,4),(4,6),(4,7),(4,13),(9,0),(9,7)]),
          ((False,True,True,False,True,True),[(3,1),(12,1)]),
          ((False,True,True,False,True,False),[(0,1),(2,1),(7,1),(8,1),(9,1)]),
          ((False,True,True,False,False,True),[(3,4)]),
          ((False,True,True,False,False,False),[(8,4)]),
          ((False,True,False,True,True,True),[(12,8),(12,10)]),
          ((False,True,False,True,True,False),[(0,8),(1,8),(2,8),(7,8),(9,8)]),
          ((False,True,False,True,False,True),[(9,10)]),
          ((False,True,False,True,False,False),[(1,4)]),
          ((False,True,False,False,True,True),[(12,4),(12,6),(12,11),(12,12),(12,13)]),
          ((False,True,False,False,True,False),[(2,4),(9,11)]),
          ((False,True,False,False,False,True),[(9,12)]),
          ((False,True,False,False,False,False),[(0,4),(7,4),(9,4),(9,6),(9,13)]),
          ((False,False,True,True,True,True),[(11,10),(11,12)]),
          ((False,False,True,True,True,False),[(2,0),(2,7),(11,0),(11,6),(11,7),(11,11),(11,13)]),
          ((False,False,True,True,False,True),[(3,0),(3,7),(3,10),(8,10)]),
          ((False,False,True,True,False,False),[(0,0),(0,7),(1,0),(1,7),(7,0),(7,7),(8,0),(8,7)]),
          ((False,False,True,False,True,True),[(3,11)]),
          ((False,False,True,False,True,False),[(8,11)]),
          ((False,False,True,False,False,True),[(3,6),(3,12),(3,13),(8,12)]),
          ((False,False,True,False,False,False),[(8,6),(8,13)]),
          ((False,False,False,True,True,True),[(2,10)]),
          ((False,False,False,True,True,False),[(1,11)]),
          ((False,False,False,True,False,True),[(0,10),(1,10),(1,12),(7,10)]),
          ((False,False,False,True,False,False),[(1,6),(1,13)]),
          ((False,False,False,False,True,True),[(2,12)]),
          ((False,False,False,False,True,False),[(0,11),(2,6),(2,11),(2,13),(7,11)]),
          ((False,False,False,False,False,True),[(0,12),(7,12)]),
          ((False,False,False,False,False,False),[(0,6),(0,13),(7,6),(7,13)])]
