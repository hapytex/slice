module Data.Range where

import Data.Slice(Slice, getIndices)

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

integralNumberOfItems :: Integral a => Range a -> Maybe a
integralNumberOfItems = undefined

realFracNumberOfItems :: RealFrac a => Range a -> Maybe a
realFracNumberOfItems Range { rStart=s, rEnd=e, rStep=st } = floor ((e-x) / st)

-- items :: Num a => Range a -> Range a
-- items =

sliceRange :: Num a => Slice a -> Range a -> Range a
sliceRange sl Range { rStart=rS, rEnd=rE, rStep=rSt } =
