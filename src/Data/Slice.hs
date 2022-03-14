{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Slice where

import Data.Maybe(fromMaybe, maybe)
import Data.Range(Range)

data Slice a =
  Slice {
      slFrom :: Maybe a
    , slTo :: Maybe a
    , slStep :: Maybe a
    } deriving (Eq, Read, Show)

showSlice :: Show a => Slice a -> String
showSlice (Slice sa sb sc) = go sa (':' : go sb (':' : go sc ""))
  where go Nothing = id
        go (Just x) = (show x ++)

getIndex :: Num a => a -> Maybe a -> Maybe a
getIndex n = fmap go
  where go i | i < 0 = n + i
             | otherwise = i

getNumStep :: Num a => Slice a -> a
getNumStep = fromMaybe 1 . slStep

isReverse :: (Num a, Ord a) => Slice a -> Bool
isReverse = (0 >) . getNumStep

isForward :: (Num a, Ord a) => Slice a -> Bool
isForward = (0 <) . getNumStep

getLowerUpper :: Num a => Slice a -> a -> Maybe (a, a)
getLowerUpper sl n
  | step < 0 = Just (-1, n-1)
  | step > 0 = Just (0, n)
  | otherwise = Nothing
  where step = getNumStep

getIndices :: Integral a => Slice a -> a -> Maybe (a, a, a)
getIndices sl@{ slFrom=f, slTo=t } n
  | stepDir = Nothing  -- forward
  | step < 0 = Nothing  -- backward
  | otherwise = Just (start, stop, step)
  where step = getNumStep sl
        stepDir = step > 0
        lower = bool (-1) 0 stepDir
        upper = n + lower
        ~(sta, sto) = bool id swap stepDir (lower, upper)
        start = fromMaybe sta (getIndex n f)
        stop = fromMaybe sto (getIndex n t)

-- stepNegative :: Num a => Slice a -> a

class Slicable a b where
    slice :: a -> Slice b -> a
