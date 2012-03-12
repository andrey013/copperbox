{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  DirectoryMetrics.CountingMap
-- Copyright   :  (c) Stephen Peter Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  to be determined
--
-- IntMap for counting frequency distributions.
--
-----------------------------------------------------------------------------


module DirectoryMetrics.CountingMap
  (

    CountingMap(..)
  , insert1
  , insert1More

  , toIntMap

  ) where

import qualified Data.IntMap as IM
import Data.Monoid

newtype CountingMap = CountingMap { getCountingMap :: IM.IntMap Int }
  deriving (Eq,Show)

toIntMap :: CountingMap -> IM.IntMap Int
toIntMap = getCountingMap

insert1 :: Int -> CountingMap -> CountingMap
insert1 idx cm = 
    CountingMap $ IM.insertWith (+) idx 1 $ getCountingMap cm

insert1More :: Int -> Int -> CountingMap -> CountingMap
insert1More idx mores cm = 
    CountingMap $ IM.insertWith (+) idx mores $ getCountingMap cm


instance Monoid CountingMap where
  mempty  = CountingMap $ mempty
  mappend = merge


merge :: CountingMap -> CountingMap -> CountingMap
merge a b = IM.foldWithKey insert1More a $ getCountingMap b
