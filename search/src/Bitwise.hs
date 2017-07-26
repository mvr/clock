{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE BinaryLiterals  #-}
module Overlay where

import Data.SBV
import Data.SBV.Examples.Misc.Word4
import Data.Monoid
import Data.Foldable (traverse_)

data HRing = HRing SWord16 deriving (Eq, Show)
data VRing = VRing SWord16 deriving (Eq, Show)

data Layer = Layer VRing HRing VRing deriving (Eq, Show)

rotateLayer :: Int -> Layer -> Layer
rotateLayer n (Layer (VRing v1) (HRing h) (VRing v2))
  = Layer (VRing $ rotateR v1 n) (HRing $ rotateR h n) (VRing $ rotateR v2 n)

--sRotateLayer :: Int -> Layer -> Layer
sRotateLayer :: (SIntegral b, SDivisible (SBV b)) =>
                SBV b -> Overlay.Layer -> Overlay.Layer
sRotateLayer n (Layer (VRing v1) (HRing h) (VRing v2))
  = Layer (VRing $ sRotateRight v1 n) (HRing $ sRotateRight h n) (VRing $ sRotateRight v2 n)

-- from left to right
data Output = Output { lv1 :: SWord8,
                       lh  :: SWord8,
                       lv2 :: SWord8,
                       rv1 :: SWord8,
                       rh  :: SWord8,
                       rv2 :: SWord8 }
  deriving (Eq, Show)

instance Monoid Layer where
  mempty = Layer (VRing zeroBits) (HRing zeroBits) (VRing zeroBits)
  (Layer (VRing v1) (HRing h) (VRing v2)) `mappend` (Layer (VRing v1') (HRing h') (VRing v2'))
    = (Layer (VRing $ v1 .|. v1') (HRing $ h .|. h') (VRing $ v2 .|. v2'))

instance Monoid Output where
  mempty = Output zeroBits zeroBits zeroBits zeroBits zeroBits zeroBits
  (Output lv1 lh lv2 rv1 rh rv2) `mappend` (Output lv1' lh' lv2' rv1' rh' rv2')
    = Output (lv1 .|. lv1') (lh .|. lh') (lv2 .|. lv2') (rv1 .|. rv1') (rh .|. rh') (rv2 .|. rv2')

outputSEq :: Output -> Output -> SBool
outputSEq (Output lv1 lh lv2 rv1 rh rv2) (Output lv1' lh' lv2' rv1' rh' rv2')
  = (lv1 .== lv1') &&& (lh .== lh') &&& (lv2 .== lv2') &&& (rv1 .== rv1') &&& (rh .== rh') &&& (rv2 .== rv2')



topFour :: SWord16
topFour = literal $ bit 15 + bit 14 + bit 13 + bit 12
topThree :: SWord16
topThree = literal $ bit 15 + bit 14 + bit 13

fstsplit8 :: SWord8 -> SWord4
fstsplit8 = undefined

getTop :: SWord16 -> SWord8
getTop w = shiftR l 4
  where (l, r :: SWord8) = split w

getMid :: SWord16 -> SWord8
getMid w = shiftR r 4
  where (l, r) = split w

layerDisplaying :: Layer -> Output
layerDisplaying (Layer (VRing v1) (HRing h) (VRing v2))
  = Output (getTop v1)
           (getTop h)
           (getTop v2)
           (getMid v2)
           (getMid h)
           (getMid v1)

digitToSegments :: Int -> (SWord8, SWord8, SWord8)
digitToSegments 0 = (literal $ 0b0110, literal $ 0b1010, literal $ 0b0110)
digitToSegments 1 = (literal $ 0b0000, literal $ 0b0000, literal $ 0b0110)
digitToSegments 2 = (literal $ 0b0010, literal $ 0b1110, literal $ 0b0100)
digitToSegments 3 = (literal $ 0b0000, literal $ 0b1110, literal $ 0b0110)
digitToSegments 4 = (literal $ 0b0100, literal $ 0b0100, literal $ 0b0110)
digitToSegments 5 = (literal $ 0b0100, literal $ 0b1110, literal $ 0b0010)
digitToSegments 6 = (literal $ 0b0110, literal $ 0b1110, literal $ 0b0010)
digitToSegments 7 = (literal $ 0b0000, literal $ 0b1000, literal $ 0b0110)
digitToSegments 8 = (literal $ 0b0110, literal $ 0b1110, literal $ 0b0110)
digitToSegments 9 = (literal $ 0b0100, literal $ 0b1110, literal $ 0b0110)
digitToSegments _ = digitToSegments 8

digitsToOutput :: (Int, Int) -> Output
digitsToOutput (l, r) = (Output lv1 lh lv2 rv1 rh rv2)
  where (lv1, lh, lv2) = digitToSegments l
        (rv1, rh, rv2) = digitToSegments r

data Clock = Clock [Layer]

clockIndices :: Clock -> [[Int]]
clockIndices c@(Clock ls) = sequence $ replicate layerCount [0 .. 15]
  where layerCount = length ls

clockDisplaying :: Clock -> [Int] -> Output
clockDisplaying (Clock ls) ixs = mconcat $ fmap (\(l, ix) -> layerDisplaying $ rotateLayer ix l) $ zip ls ixs

sClockDisplaying :: Clock -> [SWord4] -> Output
sClockDisplaying (Clock ls) ixs = do
  mconcat $ fmap (\(l, ix) -> layerDisplaying $ sRotateLayer ix l) $ zip ls ixs

canDisplay :: Clock -> (Int, Int) -> SBool
canDisplay c lr = bAny (\ixs -> goal `outputSEq` clockDisplaying c ixs) (clockIndices c)
  where goal = digitsToOutput lr

sCanDisplay :: Clock -> (Int, Int) -> Symbolic SBool
sCanDisplay c@(Clock ls) lr = do
  ixs <- mkExistVars (length ls)
  -- (flip traverse_) ixs $ \ix -> do
  --  constrain $ ix .>= 0
  --  constrain $ ix .< 16
  return $ goal `outputSEq` sClockDisplaying c ixs
  where goal = digitsToOutput lr

makeLayer = do
  v1 <- exists_
  h <- exists_
  v2 <- exists_
  return (Layer (VRing v1) (HRing h) (VRing v2))

allDigits = do
  l <- [0..9]
  r <- [0..5]
  return (l, r)

findClock l = satWith (z3{verbose=True}) $ do
  ls <- traverse (const $ makeLayer) [1 .. l]
  let c = Clock ls
  canRepresent <- traverse (\lr -> sCanDisplay c lr) allDigits
  let zeroes = replicate l 0
      symmetryBreaker = digitsToOutput (0, 0) `outputSEq` clockDisplaying c zeroes
  return $ symmetryBreaker &&& bAnd canRepresent
