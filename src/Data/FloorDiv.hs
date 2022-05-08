{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleInstances, KindSignatures, MultiParamTypeClasses #-}

module Data.FloorDiv where

import GHC.TypeLits(Nat)

class Num a => FloorDiv (n :: Nat) a where
  floorDiv :: Integral b => a -> a -> b

instance {-# Overlapping #-} Integral a => FloorDiv 0 a where
  floorDiv a = fromIntegral . div a

instance {-# Overlappable #-} RealFrac a => FloorDiv 1 a where
  floorDiv a = floor . (a /)
