{-# LANGUAGE AllowAmbiguousTypes, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, UndecidableInstances #-}

module Data.Range where

import Data.FloorDiv(FloorDiv(floorDiv))
import Data.Indicable(Indicable((!), (!?)))
import Data.Slice(Slice(Slice), getIndices)

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

numberOfItems' :: forall n a b . (FloorDiv n a, Integral b) => Range a -> b
numberOfItems' (Range s e st) = floorDiv @n (e-s) st

numberOfItems :: forall n a b . (FloorDiv n a, Integral b) => Range a -> b
numberOfItems = max 0 . numberOfItems' @n

elementAt :: (Num a, Integral i) => Range a -> i -> a
elementAt ~(Range s _ st) i = s + fromIntegral i * st

toSlice :: Range a -> Slice a
toSlice (Range s e st) = Slice (Just s) (Just e) (Just st)

getRangeIndices :: Integral a => Slice a -> a -> Maybe (Range a)
getRangeIndices sl n = (\(x, y, z) -> Range x y z) <$> getIndices sl n

instance FloorDiv n a => Indicable Range a where
  (!) = elementAt
  (!?) r i
    | i < 0 || i >= fromIntegral (numberOfItems' @n r :: Integer) = Nothing
    | otherwise = Just (elementAt r i)

sliceRange :: Num a => Slice a -> Range a -> Range a
sliceRange sl Range { rStart=rS, rEnd=rE, rStep=rSt } = undefined
