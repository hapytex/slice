module Data.RangeThen where

import Data.Range(Range(Range))

data RangeThen a
  = RangeThen {
        rtStart :: a
      , rtThen :: a
      , rtEnd :: a
  } deriving (Eq, Read, Show)

rangeTo :: (Enum a, Monoid a) => a -> RangeThen a
rangeTo = RangeThen mempty (succ mempty)

rangeFromTo :: Enum a => a -> a -> RangeThen a
rangeFromTo = RangeThen <*> succ

rangeFromThenTo :: a -> a -> a -> RangeThen a
rangeFromThenTo = RangeThen

rangeFromToStep :: Num a => a -> a -> a -> RangeThen a
rangeFromToStep f t s = RangeThen f (f+s) t

toRangeThen :: Num a => Range a -> RangeThen a
toRangeThen (Range s t d) = RangeThen s (s+d) t

fromRangeThen :: Num a => RangeThen a -> Range a
fromRangeThen (RangeThen s t e) = Range s e (t-s)
