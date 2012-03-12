{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  DirectoryMetrics.SizeMetrics
-- Copyright   :  (c) Stephen Peter Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  to be determined
--
-- Extract size metrics.
--
-----------------------------------------------------------------------------


module DirectoryMetrics.SizeMetrics
  (

    SizeMetrics(..)

  , sizeMetrics
  , sizeMetricsF

  ) where

import DirectoryMetrics.CountingMap
import DirectoryMetrics.HierSyntax

import Data.Monoid

--
-- Counting metrics can be /synthesized/ as per an attribute 
-- grammar.
--
-- Depth metrics (e.g. max_depth) must be /inherited/.
--
-- Note - Windows counts numbers of @\<DIR\>@ for the directory 
-- count at the bottom of a listing.
--
-- This is not the same as our subdir_count.
-- (File count should match)
--

data SizeMetrics = SizeMetrics
    { sz_file_count     :: !Int          -- count of files
    , sz_subdir_count   :: !Int
    , sz_file_counts    :: CountingMap
    , sz_subdir_counts  :: CountingMap
    }
  deriving Show

instance Monoid SizeMetrics where
  mempty = SizeMetrics 0 0 mempty mempty

  SizeMetrics a1 b1 c1 d1 `mappend` SizeMetrics a2 b2 c2 d2 = 
    SizeMetrics (a1+a2) (b1+b2) (c1 `mappend` c2) (d1 `mappend` d2)

sizeMetrics :: Directory -> SizeMetrics
sizeMetrics d = foldr mappend m0 $ sizeMetricsF $ dir_subdirs d
  where
    files_count      = length $ dir_files d
    subdirs_count    = length $ dir_subdirs d 
    m0 = SizeMetrics { sz_file_count    = files_count
                     , sz_subdir_count  = subdirs_count
                     , sz_file_counts   = insert1 files_count mempty
                     , sz_subdir_counts = insert1 subdirs_count mempty
                     }


sizeMetricsF :: [Directory] -> [SizeMetrics]
sizeMetricsF = map sizeMetrics