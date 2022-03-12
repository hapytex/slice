{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Slice where

import Data.Maybe(fromMaybe)
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

getNumStep :: Num a => Slice a -> a
getNumStep = fromMaybe 1 . slStep

getIndices :: Integral a => Slice a -> a -> Maybe (a, a, a)
getIndices = undefined

-- stepNegative :: Num a => Slice a -> a

class Slicable a b where
    slice :: a -> Slice b -> a
