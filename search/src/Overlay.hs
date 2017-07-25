{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveAnyClass      #-}
module Overlay where

import Data.SBV
import Data.Monoid

data Segment = Black | White | Transparent
mkSymbolicEnumeration ''Segment
type SSegment = SBV Segment

black, white, transparent :: SSegment
black = literal Black
white = literal White
transparent = literal Transparent


data HRing = HRing [SSegment] deriving (Eq, Show)
data VRing = VRing [SSegment] deriving (Eq, Show)

data Layer = Layer VRing HRing VRing deriving (Eq, Show)

rotateList :: Int -> [a] -> [a]
rotateList _ [] = []
rotateList n xs = zipWith const (drop n (cycle xs)) xs

rotateLayer :: Int -> Layer -> Layer
rotateLayer n (Layer (VRing v1) (HRing h) (VRing v2))
  = Layer (VRing $ rotateList n v1) (HRing $ rotateList n h) (VRing $ rotateList n v2)

-- from left to right
data OutputF a = Output { lv1 :: (a, a, a, a),
                       lh  :: (a, a, a),
                       lv2 :: (a, a, a, a),
                       rv1 :: (a, a, a, a),
                       rh  :: (a, a, a),
                       rv2 :: (a, a, a, a) }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
type Output = OutputF SSegment

instance Monoid Segment where
  mempty = Transparent
  a `mappend` Transparent = a
  _ `mappend` b = b

instance Monoid SSegment where
  mempty = transparent
  a `mappend` b = ite (b .== transparent) a b

instance Monoid Layer where
  (Layer (VRing v1) (HRing h) (VRing v2)) `mappend` (Layer (VRing v1') (HRing h') (VRing v2'))
    = (Layer (VRing $ v1 <> v1') (HRing $ h <> h') (VRing $ v2 <> v2'))

instance Monoid Output where
  mempty = Output (transparent, transparent, transparent, transparent)
                  (transparent, transparent, transparent)
                  (transparent, transparent, transparent, transparent)
                  (transparent, transparent, transparent, transparent)
                  (transparent, transparent, transparent)
                  (transparent, transparent, transparent, transparent)
  (Output lv1 lh lv2 rv1 rh rv2) `mappend` (Output lv1' lh' lv2' rv1' rh' rv2')
    = Output (lv1 <> lv1') (lh <> lh') (lv2 <> lv2') (rv1 <> rv1') (rh <> rh') (rv2 <> rv2')

finalSegment :: SSegment -> SSegment
finalSegment s = white <> s

finalOutput :: Output -> Output
finalOutput = fmap finalSegment

outputSEq :: Output -> Output -> SBool
outputSEq (Output lv1 lh lv2 rv1 rh rv2) (Output lv1' lh' lv2' rv1' rh' rv2')
  = (lv1 .== lv1') &&& (lh .== lh') &&& (lv2 .== lv2') &&& (rv1 .== rv1') &&& (rh .== rh') &&& (rv2 .== rv2')

layerDisplaying :: Layer -> Output
layerDisplaying (Layer (VRing v1) (HRing h) (VRing v2))
  = Output (v1 !! 0, v1 !! 1, v1 !! 2, v1 !! 3)
           (h  !! 0, h  !! 1, h  !! 2)
           (v2 !! 0, v2 !! 1, v2 !! 2, v2 !! 3)

           (v2 !! h0, v2 !! h1, v2 !! h2, v2 !! h3)
           (h  !! h0, h  !! h1, h  !! h2)
           (v1 !! h0, v1 !! h1, v1 !! h2, v1 !! h3)
  where halfway = length v1 `quot` 2
        [h0, h1, h2, h3] = fmap (halfway+) [0..3]

digitToSegments :: Int -> ((SSegment, SSegment, SSegment, SSegment),
                           (SSegment, SSegment, SSegment),
                           (SSegment, SSegment, SSegment, SSegment))
digitToSegments 0 = ((white, black, black, white), (black, white, black), (white, black, black, white))
digitToSegments 1 = ((white, white, white, white), (white, white, white), (white, black, black, white))
digitToSegments 2 = ((white, white, black, white), (black, black, black), (white, black, white, white))
digitToSegments 3 = ((white, white, white, white), (black, black, black), (white, black, black, white))
digitToSegments 4 = ((white, black, white, white), (white, black, white), (white, black, black, white))
digitToSegments 5 = ((white, black, white, white), (black, black, black), (white, white, black, white))
digitToSegments 6 = ((white, black, black, white), (black, black, black), (white, white, black, white))
digitToSegments 7 = ((white, white, white, white), (black, white, white), (white, black, black, white))
digitToSegments 8 = ((white, black, black, white), (black, black, black), (white, black, black, white))
digitToSegments 9 = ((white, black, white, white), (black, black, black), (white, black, black, white))
digitToSegments _ = digitToSegments 8

digitsToOutput :: (Int, Int) -> Output
digitsToOutput (l, r) = (Output lv1 lh lv2 rv1 rh rv2)
  where (lv1, lh, lv2) = digitToSegments l
        (rv1, rh, rv2) = digitToSegments r

data Clock = Clock [Layer]

clockSize :: Clock -> Int
clockSize (Clock (Layer (VRing r) _ _ : _)) = length r

clockIndices :: Clock -> [[Int]]
clockIndices c@(Clock ls) = sequence $ replicate layerCount [0 .. (size - 1)]
  where size = clockSize c
        layerCount = length ls

clockDisplaying :: Clock -> [Int] -> Output
clockDisplaying (Clock ls) ixs = finalOutput $ mconcat $ fmap (\(l, ix) -> layerDisplaying $ rotateLayer ix l) $ zip ls ixs

canDisplay :: Clock -> (Int, Int) -> SBool
canDisplay c lr = bAny (\ixs -> goal `outputSEq` clockDisplaying c ixs) (clockIndices c)
  where goal = digitsToOutput lr

makeLayer s = do
  v1 <- mkExistVars s
  h <- mkExistVars s
  v2 <- mkExistVars s
  return (Layer (VRing v1) (HRing h) (VRing v2))

findClock s l = allSat $ do
  ls <- traverse (const $ makeLayer s) [1 .. l]
  let c = Clock ls
  return (canDisplay c (1, 2))
