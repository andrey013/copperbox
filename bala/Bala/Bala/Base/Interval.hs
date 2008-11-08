{-# LANGUAGE MultiParamTypeClasses #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.Perform
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Interval representation
--
--------------------------------------------------------------------------------



module Bala.Base.Interval where

import Bala.Base.BaseExtra
import Bala.Base.Pitch


--------------------------------------------------------------------------------
-- Datatypes
--------------------------------------------------------------------------------


-- | Represent intervals as a pair of integers.
-- To make interval addition and especially subtraction easier, the integers
-- are wrapped by 'Count'. Subtraction does not produce negative intervals! 
data Interval = Interval { 
    -- | Count of pitch letters, it is /retrograde/ on the first note.
    -- i.e. C to C - a unison - counts 1 (rather than 0), 
    --      C to D - a second - counts 2, etc.
    _interval_type :: Int,
    -- | The number of half steps between pitched values. 
    _half_steps :: Semitone 
  }
  deriving (Eq,Show)

newtype IntervalPattern = IntervalPattern { unIntervalPattern :: [Interval] }
  deriving (Show)
  
-- | Selector for @half_steps@ (semitone count) of an @Interval@.
halfSteps :: Interval -> Semitone
halfSteps = _half_steps

  



instance Magnitude Pitch Interval where
  increase p = (p `increase`) . halfSteps
  decrease = error $ "Magnitude Pitch Interval - decrease"

buildIntervalPattern :: Intervals -> IntervalPattern
buildIntervalPattern f = IntervalPattern $ f []

type Intervals = [Interval] -> [Interval]



whole_step = (:) (Interval 2 2)  
half_step  = (:) (Interval 2 1)

