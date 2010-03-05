{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.BeamExtremity
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Beam extremity...
-- Probably this will be renamed. Printable bema groups should
-- only begin and end on (notes, chords) and not rests. 
--
--------------------------------------------------------------------------------

module Neume.BeamExtremity 
  ( 
    BeamExtremity(..)
  , NumMeasured(..)

  ) where



class BeamExtremity a where 
   rendersToNote :: a -> Bool
   
-- This is the Measured class from the FingerTree paper and 
-- library but with Num for the superclass rather than Monoid

class Num (Measurement a) => NumMeasured a where
  type Measurement a
  nmeasure :: a -> Measurement a
