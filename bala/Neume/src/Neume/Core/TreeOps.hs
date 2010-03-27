{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.TreeOps
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Operations on the 'PletTree'.
--
--------------------------------------------------------------------------------

module Neume.Core.TreeOps
  (
  -- * Operations
    pletFold
  , pletAll
  , pletMeasure
  , pletCount

  ) where

import Neume.Core.Datatypes
import Neume.Core.Metrical
import Neume.Core.Duration

import Data.List ( foldl' )


pletFold :: (a -> b -> b) -> (PletMult -> b -> b) -> b -> PletTree a -> b
pletFold f _ b (S a)        = f a b
pletFold f g b (Plet pm xs) = foldl' (pletFold f g) (g pm b) xs


pletAll :: (a -> Bool) -> PletTree a -> Bool
pletAll test (S a)          = test a
pletAll test (Plet _ notes) = step notes where
   step []                      = True
   step (p:ps) | pletAll test p = step ps
   step _                       = False


--------------------------------------------------------------------------------
-- Measuring the plet-tree, and tuplet stack


-- | The measure of a \single\ or a \plet tree\ - plet trees are
-- considered indivisable so it is not a problem to sum them.
--
pletMeasure :: (Measurement a ~ DurationMeasure, NumMeasured a) 
            => PletTree a -> DurationMeasure
pletMeasure = snd . pletFold  phi chi (mult_stack_zero,0) where
  phi a  (stk,acc) = (stk, acc + nmeasureCtx stk a)
  chi pm (stk,acc) = (pushPM pm stk,acc) 



--------------------------------------------------------------------------------

-- | The number of items in a PletTree 
--
-- NOTE - need the pred to test e.g. grace notes, which 
-- shouldn\'t be counted.
--
-- Is this a suitable case for another Type Class?
--
pletCount :: (a -> Bool) -> PletTree a -> Int
pletCount test = pletFold phi chi 0 where
  phi a n  | test a    = n+1
           | otherwise = n
  chi _ n              = n

