{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Basic.Primitive.Syntax
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

module ZMidi.Basic.Primitive.Syntax
  ( 
   
    EventList
  , Locale
  , Primitive
  , Event1

  , frame
  , eventGroup
  , instant
  , onoff

  -- * Extract delta time MIDI messages
  , extractMessages

  ) where


import ZMidi.Basic.Primitive.TimeSpan
import ZMidi.Basic.Primitive.Transform
import ZMidi.Basic.Utils.HList
import ZMidi.Basic.Utils.JoinList
import qualified ZMidi.Basic.Utils.JoinList as JL

import ZMidi.Core                               -- package: zmidi-core


import qualified Data.Foldable as F
import Data.List ( sort )
import Data.Monoid
import Data.Word

-- | Unlike Wumpus, there is no need to support nesting, so we 
-- can use a tree rather than a list.
--
data EventList = EventList Locale (JoinList Primitive)
   deriving (Show)

type instance DUnit EventList = Double

type Locale = DTimeSpan

type StartDelta = Double

-- | The event_seq is not necessarily ordered and the deltas
-- are distance from the event start to the event, 
-- \*\* they are not \*\* deltas between consecutive event.
-- 
-- This representation is perhaps a bit odd but it does allow
-- cheap translation - just move event_start. It also allows
-- a simple implementation of scaling - map through the 
-- start-deltas.
--
data Primitive = EventSequence { _event_start :: Double
                               , _event_seq   :: [(StartDelta, Event1)]
                               }
  deriving (Eq,Ord,Show)

type instance DUnit Primitive = Double


data Event1 = Instant MidiEvent
            | OnOff { _on_evt        :: MidiEvent
                    , _on_duration   :: Double 
                    , _off_event     :: MidiEvent
                    }
  deriving (Eq,Ord,Show)


type instance DUnit Event1 = Double



instance Monoid EventList where
  mempty = EventList (spanInstant 0) mempty
  EventList tspan0 se0 `mappend` EventList tspan1 se1 = 
      EventList (tspan0 `spanUnion` tspan1) (se0 `JL.append` se1)


--------------------------------------------------------------------------------
-- Build

frame :: JoinList Primitive -> EventList
frame (viewl -> p0 :< ps) = EventList tspan se
  where
    (tspan,se)   = F.foldl' fn (timeBounds p0, JL.one p0) ps 
    fn (ts,jl) p = let tsp = timeBounds p in (spanUnion ts tsp, JL.snoc jl p)

frame (viewl -> _)        = EventList (spanInstant 0) mempty


eventGroup :: Double -> [(StartDelta,Event1)] -> Primitive
eventGroup = EventSequence

instant :: MidiEvent -> Event1
instant = Instant

onoff :: MidiEvent -> Double -> MidiEvent -> Event1
onoff = OnOff

{-
mergePrims :: JoinList Primitive -> EventList
mergePrims se = EventList min_onset $ map 
  where
    (min_onset
-}

--------------------------------------------------------------------------------
-- Extract 


-- | Sorted plus trailing @end-of-track@ message.
--
extractMessages :: EventList -> [MidiMessage]
extractMessages (EventList _ se) = 
    step 0 $ sort $ toListH $ F.foldr fn emptyH se
  where  
    fn p ac = appendH ac $ primMessages p

    step ot ((t,e):xs) = let ut = durationr t 
                         in (fromIntegral $ ut-ot,e) : step ut xs
    step _  []         = [delta_end_of_track]


-- | Absolute time not delta time, unsorted...
--
primMessages :: Primitive -> H (Double, MidiEvent)
primMessages (EventSequence ot ss) = foldr fn emptyH ss 
  where
    fn (dt, Instant e)     ac = (ot+dt, e) `consH` ac
    fn (dt, OnOff e0 d e1) ac = (ot+dt, e0) `consH` (ot+dt+d, e1) `consH` ac



-- Note - this is a Double so it is the right type for calculating
-- durations, in MIDI files the size of the ticks-per-beat 
-- designator is actually a Word16.
--
ticks_per_quarternote :: Double
ticks_per_quarternote = 480

durationr :: Double -> Word32
durationr r = floor $ (4 * ticks_per_quarternote) * r

delta_end_of_track :: MidiMessage
delta_end_of_track = (0, MetaEvent $ EndOfTrack)


--------------------------------------------------------------------------------
-- Boundaries


-- | Boundary is cached.
--
instance TimeBounds EventList where
  timeBounds (EventList tspan _) = tspan


instance TimeBounds Primitive where
  timeBounds = boundaryPrimitive

boundaryPrimitive :: Primitive -> DTimeSpan
boundaryPrimitive (EventSequence s es) = TimeSpan s (step 0 es)
  where
    step d []                        = d
    step d ((delta, Instant {} ):xs) = step (max d delta) xs
    step d ((delta, OnOff _ t _):xs) = step (max d (delta+t)) xs 


--------------------------------------------------------------------------------
-- Transform

instance Translate EventList where
  translate dt (EventList tspan se) = 
      EventList (translate dt tspan) (fmap (translate dt) se)

instance SReverse EventList where
  sreverse         = sreverseEventList


instance Scale EventList where
  scale sx (EventList tspan se) = 
      EventList (scale sx tspan) (fmap (scale sx) se)


instance Reposition EventList where
  reposition ot (EventList tspan se) = 
      EventList (reposition ot tspan) (fmap (reposition ot) se)


instance Translate Primitive where
  translate         = translatePrim


instance SReverse Primitive where
  sreverse          = sreversePrim

instance Scale Primitive where
  scale             = scalePrim

instance Reposition Primitive where
  reposition        = repositionPrim


sreverseEventList :: EventList -> EventList
sreverseEventList a@(EventList tspan se)  = 
    EventList tspan $ fmap (fn . sreverse) se
  where
    dmax = spanDuration $ timeBounds a    
    fn (EventSequence ot es) = EventSequence (dmax - ot) es

 
translatePrim :: Double -> Primitive -> Primitive
translatePrim dt (EventSequence s es) = EventSequence (s + dt) es


sreversePrim :: Primitive -> Primitive
sreversePrim p@(EventSequence s es) = 
    EventSequence s $ fmap (\(d,e) -> (dmax - d, e)) es
  where
    dmax = spanDuration $ timeBounds p

scalePrim :: Double -> Primitive -> Primitive
scalePrim sx (EventSequence s es) = 
    EventSequence (s * sx) $ map (\(d,e) -> (d * sx, scale sx e)) es


repositionPrim :: Double -> Primitive -> Primitive
repositionPrim ot (EventSequence _ se) = EventSequence ot se


instance Scale Event1 where 
  scale             = scaleEvent1

scaleEvent1 :: Double -> Event1 -> Event1
scaleEvent1 _  (Instant e)     = Instant e
scaleEvent1 sx (OnOff e0 d e1) = OnOff e0 (d * sx) e1
