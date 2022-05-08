module Data.RangeThen where

data RangeThen a
  = RangeThen {
        rtStart :: a
      , rtThen :: a
      , rtEnd :: a
  } deriving (Eq, Read, Show)
