module Vertical where

import Data.SBV

type Ring = [SBool]

isRepresenting :: (SBool, SBool, SBool, SBool) -> Ring -> Ring -> SBool
isRepresenting (l1, l2, r1, r2) ring1 ring2
  =   (((ring1 !! 0) ||| (ring2 !! 0))           .== l1)
  &&& (((ring1 !! 1) ||| (ring2 !! 1))           .== l2)
  &&& (((ring1 !! h1) ||| (ring2 !! h1))         .== r2)
  &&& (((ring1 !! h2) ||| (ring2 !! h2))         .== r1)
  where halfway = length ring1 `quot` 2
        h1 = halfway
        h2 = halfway + 1

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
