{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Data.Slice where

import Data.Bool(bool)
import Data.Default.Class(Default(def))
import Data.List(genericDrop)
import Data.Maybe(fromMaybe)
import Data.Tuple(swap)

import Text.Parsec(ParsecT, Stream, option, optionMaybe)
import Text.Parsec.Char(char)

data Slice a =
  Slice {
      slFrom :: Maybe a
    , slTo :: Maybe a
    , slStep :: Maybe a
    } deriving (Eq, Read, Show)

instance Default (Slice a) where
    def = Slice Nothing Nothing Nothing

showSlice :: Show a => Slice a -> String
showSlice (Slice sa sb sc) = go sa (':' : go sb (':' : go sc ""))
  where go = maybe id ((++) . show)

parseSlice :: Stream s m Char => ParsecT s u m a -> ParsecT s u m (Slice a)
parseSlice item = Slice <$> oi <*> cc <*> option Nothing cc
  where cc = char ':' *> oi
        oi = optionMaybe item

_lowerCheck :: (Num a, Ord a) => a -> Maybe a
_lowerCheck n
  | n < 0 = Nothing
  | otherwise = Just n

getIndex :: (Num a, Ord a) => a -> (a, a) -> a -> Maybe a -> a
getIndex n ~(lo, up) = (`maybe` go)
  where go i | i < 0 = max (n + i) lo
             | otherwise = min i up

getNumStep :: Num a => Slice a -> a
getNumStep = fromMaybe 1 . slStep

isReverse :: (Num a, Ord a) => Slice a -> Bool
isReverse = (0 >) . getNumStep

isForward :: (Num a, Ord a) => Slice a -> Bool
isForward = (0 <) . getNumStep

isValidStep :: (Num a, Ord a) => Slice a -> Bool
isValidStep Slice { slStep=sl } = maybe True (0 /=) sl

validStepCheck :: (Num a, Ord a) => (Slice a -> b) -> Slice a -> Maybe b
validStepCheck f = go
  where go sl
          | isValidStep sl = Just (f sl)
          | otherwise = Nothing

getLowerUpper' :: (Num a, Ord a) => a -> Slice a -> (a, a)
getLowerUpper' n sl
  | isForward sl = (0, n)
  | otherwise = (-1, n-1)

getLowerUpper :: (Num a, Ord a) => a -> Slice a -> Maybe (a, a)
getLowerUpper = validStepCheck . getLowerUpper'

getIndices :: (Num a, Ord a) => Slice a -> a -> Maybe (a, a, a)
getIndices sl@Slice { slFrom=f, slTo=t } n
  | step /= 0 = Just (go staFb f, go stoFb t, step)
  | otherwise = Nothing
  where step = getNumStep sl
        stepDir = step > 0
        lu = getLowerUpper' n sl
        ~(staFb, stoFb) = bool id swap stepDir lu
        go = getIndex n lu

class Slicable a b where
    slice :: a -> Slice b -> a
    slice = (⋮)

    (⋮) :: a -> Slice b -> a
    (⋮) = slice
    {-# MINIMAL slice | (:) #-}

class PartialSlicable a b where
    trySlice :: a -> Slice b -> Maybe a
    trySlice = (⋮?)

    (⋮?) :: a -> Slice b -> Maybe a
    (⋮?) = trySlice
    {-# MINIMAL trySlice | (⋮?) #-}

instance {-# Overlappable #-} Slicable a b => PartialSlicable a b where
    trySlice s = Just . slice s

instance Integral b => Slicable [a] b where
    slice = undefined

isVoid :: (Num a, Ord a) => Slice a -> Bool
isVoid s@(Slice (Just bg) (Just en) _) = not (isNotVoid_ (getNumStep s) bg en)
isVoid _ = False

isNotVoid_ :: (Num a, Ord a) => a -> a -> a -> Bool
isNotVoid_ st
  | st <= 0 = (<)
  | otherwise = (>)

takeEach_ :: Integral i => i -> [a] -> [a]
takeEach_ 1 = id  -- runtime optimization
takeEach_ 0 = repeat . head
takeEach_ n = go
    where go [] = []
          go (x:xs) = x : go (go' xs)
          go' = genericDrop (abs n - 1)
