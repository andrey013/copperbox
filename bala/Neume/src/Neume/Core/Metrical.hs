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
  , BeamExtremity(..)
  , NumMeasured(..)

  ) where

type PletMult = (Int,Int)

class BeamExtremity a where 
   rendersToNote :: a -> Bool
   
-- This is the Measured class from the FingerTree paper and 
-- library but with Num for the superclass rather than Monoid

class Num (Measurement a) => NumMeasured a where
  type Measurement a
  nmeasure :: a -> Measurement a
