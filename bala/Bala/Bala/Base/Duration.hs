{-# LANGUAGE TypeSynonymInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.Duration
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Duration representation - pulls in the HNotate.Duration module
--
--------------------------------------------------------------------------------

module Bala.Base.Duration (
  module HNotate.Duration,
  TimeSig,
  showBars,
  rhychar

  ) where

import HNotate.Duration

import Bala.Base.BaseExtra (applyi)

import Data.Ratio

type TimeSig = (Int,Int)

showBars :: Int -> TimeSig -> ShowS
showBars n s = showChar '|' . applyi n (tsRender1 s . showChar '|')


-- O % & , .
-- wn, hn, qn, 8th, 16th  

tsRender1 :: TimeSig -> ShowS
tsRender1 (n,d) = applyi n ((cs d) .) id where
  cs 1  = showChar 'O'
  cs 2  = showChar '%'
  cs 4  = showChar '&'
  cs 8  = showChar ','
  cs 16 = showChar '.'
  cs _  = showChar '+' 
  

rhychar :: RhythmicValue a => a -> Char
rhychar a = let r = rhythmicValue a in fn (numerator r) (denominator r) where
  fn n d | n == 1     = ch d
         | otherwise  = '+' 
  ch  1  = 'O'
  ch  2  = '%'
  ch  4  = '&'
  ch  8  = ','
  ch 16  = '.'
  ch  _  = '+' 


 