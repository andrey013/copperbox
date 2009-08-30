{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.PitchClass
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pitch class  (modulo 12 arithmetic)
--
--------------------------------------------------------------------------------

module Bala.PitchClass where

import Bala.Modulo
  

class Transpose a where
  transpose :: a -> a -> a
  invert    :: a -> a -> a

instance Transpose Z12 where
  transpose a b = b + a
  invert a b = (negate b) + a

type Triad a = (a,a,a)

parallel :: (Transpose a, Num a) => (a,a,a) -> (a,a,a)
parallel (y1,y2,y3) = (f y1, f y2, f y3) where f = invert (y1+y3)

-- Leading tone exchange
ltExch :: (Transpose a, Num a) => (a,a,a) -> (a,a,a)
ltExch (y1,y2,y3) = (f y1, f y2, f y3) where f = invert (y2+y3)

relative :: (Transpose a, Num a) => (a,a,a) -> (a,a,a)
relative (y1,y2,y3) = (f y1, f y2, f y3) where f = invert (y1+y2)



