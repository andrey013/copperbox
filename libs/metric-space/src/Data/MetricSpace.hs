{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.MetricSpace
-- Copyright   :  (c) Stephen Peter Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  GHC 6.10+ (TypeFamilies)
--
-- MetricSpace - type class representing the notion of distance.
--
-----------------------------------------------------------------------------

module Data.MetricSpace
  ( 
  -- * Metric space typeclass   
     MetricSpace(..)
  ) where

import Data.Ratio

class MetricSpace a where
  type Distance a 
  distance :: a -> a -> Distance a



instance MetricSpace Int where
  type Distance Int = Int
  distance a b = abs $ a-b

instance MetricSpace Integer where
  type Distance Integer = Integer
  distance a b = abs $ a-b

instance MetricSpace Float where
  type Distance Float = Float
  distance a b = abs $ a-b

instance MetricSpace Double where
  type Distance Double = Double
  distance a b = abs $ a-b

instance Integral a => MetricSpace (Ratio a) where
  type Distance (Ratio a) = Ratio a
  distance a b = abs $ a-b


