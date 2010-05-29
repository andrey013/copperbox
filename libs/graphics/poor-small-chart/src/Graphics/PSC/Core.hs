{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.PSC.Core
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Core types, functions...
--
--------------------------------------------------------------------------------

module Graphics.PSC.Core
  (
  -- * Types
    PointSize
  , LineWidth
  , Range

  -- * functions
  , rescale
  , clamp
  
  , ffloat
  
  ) where

import Numeric


-- | PointSize - synonymously font size.
--
type PointSize = Int


-- | LineWidth - 1 = 1%72 of an inch.
-- 
type LineWidth = Double

-- | 'Range' @ (min,max, toDouble) @
--
type Range u = (u, u, u -> Double)

-- @rescale old_min old_max  new_min new_max  a@
--
rescale :: Fractional a => a -> a -> a -> a -> a -> a
rescale amin amax bmin bmax a = 
    bmin + offset * (output_range / input_range)  
  where
    input_range   = amax - amin
    output_range  = bmax - bmin
    offset        = a - amin 


-- | @clamp min max a@ - clamp a to be with in the bounds 
-- min..max
--
clamp :: Ord a => a -> a -> a -> a
clamp amin amax a = max amin (min amax a)



-- | As per showFFloat but makes a String...
--
ffloat :: RealFloat u => Int -> u -> String
ffloat prec = ($ "") . showFFloat (Just prec)
