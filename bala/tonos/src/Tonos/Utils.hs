{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Tonos.Utils
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Alternative representation of Pitch, Intervals etc. that 
-- (hopefully) has better defined mathematical operations.
--
--------------------------------------------------------------------------------

module Tonos.Utils
  (
    toFro
  , clamp
  , clampRem
  
  , smod

  ) where


toFro :: Enum a => (Int -> Int) -> a -> a
toFro f = toEnum . f . fromEnum


clamp :: (Num a, Ord a) => a -> a -> a -> a
clamp mini maxi n = max mini (min maxi n)

clampRem :: (Num a, Ord a) => a -> a -> a -> (a,a)
clampRem mini maxi n = (a,exc)
  where
    a = clamp mini maxi n
    exc = if a == mini then (n - a) else if a == maxi then (n - a) else 0


smod :: Integral a => a -> a -> a
smod n k = shift (n `mod` k) (k `div` 2)
  where
    shift i halfk | i > halfk = negate halfk + (i - halfk)
                  | otherwise = i
