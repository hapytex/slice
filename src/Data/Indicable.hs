{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Indicable where

import Data.Maybe(fromJust)

class Indicable f a where
  (!) :: Integral i => f a -> i -> a
  (!) fa = fromJust . (!?) fa

  (!?) :: Integral i => f a -> i -> Maybe a
  (!?) fa = Just . (!) fa
  {-# MINIMAL (!) | (!?) #-}
