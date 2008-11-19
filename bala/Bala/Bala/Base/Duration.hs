{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}


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
  unitDuration,
  showBars,
  
  divModBar,
  
  segmentByTS,
  
  XOne(..),
  xdot1, xdotAve,   manyToXOne,
  
  ) where

import Bala.Base.BaseExtra (applyi)

import HNotate.Duration
import HNotate.Fits

import qualified Data.Foldable as F
import Data.Ratio
import Data.Sequence


type TimeSig = (Int,Int)

unitDuration :: TimeSig -> Duration
unitDuration (n,d) = makeDuration n d

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
  

instance Fits Duration Duration where
  measure d     = d  
  resizeTo _  d = d


-- barfill is analoguous to divMod, but with funnier types...

divModBar :: Duration -> TimeSig -> (Int,Duration)
divModBar dn (n,d) = fn $ dn `divModR` (makeDuration n d) where
  fn (i,r) = (fromIntegral i,r)





segmentByTS :: Fits a Duration => TimeSig -> Seq a -> Seq (Seq a)
segmentByTS (n,d) se = segment (makeDuration 1 d) se  


-- Print metrical bit patterns as x.x...x...

class XOne a where xone :: a -> Bool

instance XOne Bool where
  xone = id

-- Its arbitrary whether we choose Ture of False for this instance
instance XOne Duration where
  xone _ = True

xdot1 :: XOne a => a -> Char
xdot1 a | xone a    = 'x'
        | otherwise = '.'

xdotAve :: (Fits a Duration, XOne a) => TimeSig -> Seq a -> [Char]
xdotAve tm se = 
    F.foldr (\e a -> (xdot1 $  manyToXOne e) : a) [] (segmentByTS tm se)

  
manyToXOne :: (Fits a Duration, XOne a) => Seq a -> Bool
manyToXOne se = gteHalf (sumXOne se) (sumMeasure se) where
  sumXOne = F.foldl (\a e -> if xone e then a + measure e else a) duration_zero  

  gteHalf :: Duration -> Duration -> Bool
  gteHalf a b = a >= (b / 2)
   