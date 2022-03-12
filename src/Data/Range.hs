module Data.Range where

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
