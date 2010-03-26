{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.Metrical
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Beaming and metrical types and utilities.
--
-- Extremities - printable beam groups should only begin and end 
-- on (notes, chords) and not rests. 
--
--------------------------------------------------------------------------------

module Neume.Core.Metrical
  ( 
    PletMult
  , MultiplierStack
  , mult_stack_zero
  , pushPM
  , scaleFactor
  , nmeasureCtx

  , BeamExtremity(..)
  , NumMeasured(..)

  ) where

import Neume.Core.Duration

import Data.Ratio

-- Store plet-multipliers as pairs of integers rather than a 
-- Rational as a Rational normalizes the fraction.
--
type PletMult = (Integer,Integer)

type MultiplierStack = [Rational]

mult_stack_zero :: MultiplierStack
mult_stack_zero = []


-- TODO - ensure that plet multiplier is used correctly with ABC
-- which may want the numerator / denominator in the other order
--
pushPM :: PletMult -> MultiplierStack -> MultiplierStack
pushPM (p,q) stk = (p%q) : stk

scaleFactor :: MultiplierStack -> DurationMeasure
scaleFactor []     = (1%1)
scaleFactor (x:xs) = x * scaleFactor xs


nmeasureCtx :: (Measurement a ~ DurationMeasure, NumMeasured a) 
            => MultiplierStack -> a -> DurationMeasure
nmeasureCtx stk a = nmeasure a * (scaleFactor stk)


--------------------------------------------------------------------------------



class BeamExtremity a where 
   rendersToNote :: a -> Bool
   
-- This is the Measured class from the FingerTree paper and 
-- library but with Num for the superclass rather than Monoid

class Num (Measurement a) => NumMeasured a where
  type Measurement a
  nmeasure :: a -> Measurement a
