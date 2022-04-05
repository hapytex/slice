{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleInstances, FunctionalDependencies, IncoherentInstances, KindSignatures, MultiParamTypeClasses, UndecidableInstances #-}

module Data.Range where

import Data.Indicable(Indicable((!), (!?)))
import Data.Slice(Slice(Slice), getIndices)

import GHC.TypeLits(Nat)

data Range a
  = Range {
        rStart :: a
      , rEnd :: a
      , rStep :: a
    } deriving (Eq, Read, Show)

rangeTo :: Num a => a -> Range a
rangeTo = rangeFromTo 0

rangeFromTo :: Num a => a -> a -> Range a
rangeFromTo from to = rangeFromToStep from to 1

rangeFromToStep :: a -> a -> a -> Range a
rangeFromToStep = Range

rangeFromThenTo :: Num a => a -> a -> a -> Range a
rangeFromThenTo x1 x2 xn = Range x1 xn (x2-x1)

integralNumberOfItems' :: Integral a => Range a -> a
integralNumberOfItems' ~(Range { rStart=s, rEnd=e, rStep=st }) = div (e-s) st

realFracNumberOfItems' :: (RealFrac a, Integral b) => Range a -> b
realFracNumberOfItems' ~(Range { rStart=s, rEnd=e, rStep=st }) = floor ((e-s) / st)

integralNumberOfItems :: Integral a => Range a -> a
integralNumberOfItems = max 0 . integralNumberOfItems'

realFracNumberOfItems :: (RealFrac a, Integral b) => Range a -> b
realFracNumberOfItems = max 0 . realFracNumberOfItems'

elementAt :: (Num a, Integral i) => Range a -> i -> a
elementAt ~(Range s _ st) i = s + fromIntegral i * st

toSlice :: Range a -> Slice a
toSlice (Range s e st) = Slice (Just s) (Just e) (Just st)

getRangeIndices :: Integral a => Slice a -> a -> Maybe (Range a)
getRangeIndices sl n = (\(x, y, z) -> Range x y z) <$> getIndices sl n

class FloorDiv (n :: Nat) a where
  floorDiv :: Integral b => a -> a -> b

instance {-# Overlapping #-} Integral a => FloorDiv 0 a where
  floorDiv a = fromIntegral . div a

instance {-# Overlappable #-} RealFrac a => FloorDiv 1 a where
  floorDiv a = floor . (a /)

instance FloorDiv n a => Indicable Range a where
  (!) = elementAt
  (!?) r i
    | i < 0 || i >= fromIntegral (integralNumberOfItems' r) = Nothing
    | otherwise = Just (r ! i)

sliceRange :: Num a => Slice a -> Range a -> Range a
sliceRange sl Range { rStart=rS, rEnd=rE, rStep=rSt } = undefined
