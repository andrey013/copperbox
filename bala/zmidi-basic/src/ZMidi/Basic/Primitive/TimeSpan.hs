{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Basic.Primitive.TimeSpan
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- 
--
--------------------------------------------------------------------------------

module ZMidi.Basic.Primitive.TimeSpan
  ( 

    TimeSpan(..)
  , DTimeSpan

  , TimeBounds(..)

  , timeSpan
  , spanInstant
  , spanUnion
  , traceBoundary

  , maxBoundary
  , spanDuration

  ) where


import ZMidi.Basic.Primitive.Transform

-- | TimeSpan cf. a bounding box.
--
data TimeSpan u = TimeSpan { span_start :: u, span_end :: u }
  deriving (Eq,Ord,Show)

type DTimeSpan = TimeSpan Double


type instance DUnit (TimeSpan u) = u


instance Num u => Translate (TimeSpan u) where
  translate dt (TimeSpan s e) = TimeSpan (s + dt) (e + dt)

-- | Uninterpreted - just id.
--
instance SReverse (TimeSpan u) where
  sreverse = id


instance Fractional u => Scale (TimeSpan u) where
  scale sx (TimeSpan s e) = let sx'= realToFrac sx 
                            in TimeSpan (s * sx') (e * sx')


instance Num u => Reposition (TimeSpan u) where
  reposition ot (TimeSpan s e) = let len = e - s in TimeSpan ot (ot + len)


--------------------------------------------------------------------------------

-- | Type class extracting the time frame of an object - 
-- Event, Primitive etc.
--
class TimeBounds t where
  timeBounds :: u ~ DUnit t => t -> TimeSpan u 

instance TimeBounds (TimeSpan u) where
  timeBounds = id




-- | 'timeSpan' : @start * end -> TimeSpan@
--
-- Contruct a TimeSpan, vis the TimeSPan constructor with range 
-- checking on the start and end.
--
-- 'timeSpan' throws an error if the time span is negative.
--
timeSpan :: Ord u => u -> u -> TimeSpan u
timeSpan s e 
    | s <= e    = TimeSpan s e
    | otherwise = error "TimeSpan.timeSpan - malformed."


spanInstant :: u -> TimeSpan u
spanInstant t0 = TimeSpan t0 t0 

spanUnion :: Ord u => TimeSpan u -> TimeSpan u -> TimeSpan u
spanUnion (TimeSpan s0 e0) (TimeSpan s1 e1) = 
    TimeSpan { span_start = min s0 s1, span_end = max e0 e1 }



-- | 'traceBoundary' : @ onsets -> TimeSpan @
--
-- Trace a list of onsets, returning the TimeSpan of their
-- boundary.
--
-- \*\* WARNING \*\* - 'traceBoundary' throws a run-time error
--  when supplied with the empty list.
--
traceBoundary :: Ord u => [u] -> TimeSpan u
traceBoundary (p:ps) = 
    uncurry TimeSpan $ foldr (\z (a,b) -> (min z a, max z b) ) (p,p) ps
traceBoundary []     = error $ "TimeSpan.traceBoundary called on empty list"


-- | 'maxBoundary' : @ [TimeSpan] -> TimeSpan @
--
-- Find the maxBoundary of a list of TimeSpans.
--
-- \*\* WARNING \*\* - 'traceBoundary' throws a run-time error
--  when supplied with the empty list.
--
maxBoundary :: Ord u => [TimeSpan u] -> TimeSpan u
maxBoundary (t:ts) = foldr spanUnion t ts
maxBoundary []     = error $ "TimeSpan.maxBoundary called on empty list"


-- | 'spanDuration' : @ time_span -> Duration @
--
-- Extract the duration of a TimeSpan.
--
spanDuration :: Num u => TimeSpan u -> u
spanDuration (TimeSpan s e) = e - s