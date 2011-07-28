{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Core.Timespan
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Timespan of an event (cf. bounding box in Wumpus-Core)
--
--------------------------------------------------------------------------------

module Majalan.Core.Timespan
  (

  -- * Datatype
    Timespan(..)
  , DTimespan

  -- * Operations
  , timespan
  , timespanUnion
  , traceTimespan

  , timespanCenter
  , withinTimespan
  , timespanLength
   
  ) where


import Majalan.Core.Basis

-- | This is equaivalent to a bounding box.
--
data Timespan u = Timespan
      { timespan_start  :: !u
      , timespan_end    :: !u
      }
  deriving (Show)

type instance DUnit (Timespan u) = u

type DTimespan = Timespan Double


instance Functor Timespan where
  fmap f (Timespan t0 t1) = Timespan (f t0) (f t1)


--------------------------------------------------------------------------------

-- | Type class extracting the timespan of an object - 
-- Event, Score etc.
--

-- Note - needs DUnit defined in Core rather than Basic...


class Timeframe t where
  timeframe :: u ~ DUnit t => t -> Timespan u 


instance Timeframe (Timespan u) where
  timeframe = id


--------------------------------------------------------------------------------
-- Operations

-- | 'timespan' : @start * end -> Timespan@
--
-- Contruct a timespan with range checking on the start and end 
-- points.
--
-- 'timespan' throws an error if the length of the constructed 
-- timespan is negative.
--
timespan :: (Num u, Ord u) => u -> u -> Timespan u
timespan t0 t1
    | t0 <= t1 && t0 >= 0  = Timespan t0 t1
    | otherwise            = error "Majalan.Core.Timespan.timespan - malformed."


-- | The union of two timespans. 
--
timespanUnion :: Ord u => Timespan u -> Timespan u -> Timespan u
timespanUnion (Timespan a0 a1) (Timespan b0 b1) = 
    Timespan (min a0 b0) (max a1 b1)


traceTimespan :: Ord u => [u] -> Timespan u
traceTimespan (p:ps) = 
    foldr (\z (Timespan a b) -> Timespan (min z a) (max z b)) (Timespan p p) ps

traceTimespan []     = 
    error $ "Majalan.Core.Timespan.traceTimespan called in empty list"




-- | 'timespanCenter' : @timespan -> Time@
-- 
-- Return the mid-time of a Timespan.
--
timespanCenter :: Fractional u => Timespan u -> u
timespanCenter (Timespan t0 t1) = t0 + (0.5*(t1-t0))
 

-- | 'withinTimespan' : @ time * timespan -> Bool @
-- 
-- Within test - is the supplied time within the timespan?
--
withinTimespan :: Ord u => u -> Timespan u -> Bool
withinTimespan t (Timespan t0 t1) = t >= t0 && t <= t1


-- | 'timespanLength' : @ timespan -> u @
--
-- Extract the length of a timespan.
--
timespanLength :: Num u => Timespan u -> u 
timespanLength (Timespan t0 t1) = t1 - t0