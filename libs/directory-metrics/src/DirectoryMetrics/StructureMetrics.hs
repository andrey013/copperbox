{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  DirectoryMetrics.StructureMetrics
-- Copyright   :  (c) Stephen Peter Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  to be determined
--
-- Extract structure metrics.
--
-----------------------------------------------------------------------------


module DirectoryMetrics.StructureMetrics
  (

    StructMetrics(..)

  , structMetrics
  , structMetricsF

  ) where

import DirectoryMetrics.HierSyntax

import Control.Applicative
import Data.Monoid

--
-- Counting metrics can be /synthesized/ as per an attribute 
-- grammar.
--
-- Depth metrics (e.g. max_depth) must be /inherited/.
-- (Is this true? - no can be synthesized)
--

data StructMetrics = StructMetrics
    { str_max_depth     :: !Int          -- max depth of files
    }
  deriving Show

instance Monoid StructMetrics where
  mempty = StructMetrics 0

  StructMetrics a1 `mappend` StructMetrics a2 = 
    StructMetrics (max a1 a2)


structMetrics :: Directory -> StructMetrics
structMetrics d = update m1
  where
    update = (\s i -> s { str_max_depth = i+1}) <*> str_max_depth
    m1     = mconcat $ m0 : (structMetricsF $ dir_subdirs d)
    m0     = StructMetrics 0

structMetricsF :: [Directory] -> [StructMetrics]
structMetricsF = map structMetrics

