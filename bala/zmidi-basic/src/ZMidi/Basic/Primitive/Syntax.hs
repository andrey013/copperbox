{-# LANGUAGE TypeFamilies               #-}
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
  , Event1

  , eventList
  , eventList1
  , instant
  , onoff
  
  , consec
  , instantEL
  , onoffEL
  
  , durationEL
  -- * Extract delta time MIDI messages
  , extractMessages
  

  ) where


import ZMidi.Basic.Primitive.Transform
import ZMidi.Basic.Utils.HList
import ZMidi.Basic.Utils.JoinList
import qualified ZMidi.Basic.Utils.JoinList as JL

import ZMidi.Core                               -- package: zmidi-core


import qualified Data.Foldable as F
import Data.List ( sort )
import Data.Monoid
import Data.Word




type StartDelta = Double


-- sreverse 
-- sreverse [(10,A), (12,B)] == [(0,B), (2,A)]

-- scale 
-- scale 0.5 [(10,A), (12,B)] == [(5,A), (6,B)]

-- translate 
-- translate 5 [(10,A), (12,B)] == [(15,A), (17,B)]

-- mappend 
-- [(10,A), (12,B)] `mappend`  [(5,C)] == [(5,C), (10,A), (12,B)]

-- consecutive
-- [(10,A), (12,B)] `consec`  [(5,C)] == [(10,A), (12,B), (17,C)]


-- | The @list_events@ is not necessarily ordered and the deltas
-- are distance from the event start to the event, 
-- \*\* they are not \*\* deltas between consecutive event.
-- 
-- This representation is perhaps a bit odd but it does allow
-- cheap translation - just move event_start. It also allows
-- a simple implementation of scaling - map through the 
-- start-deltas.
--
data EventList = EventList { list_len    :: Double
                           , list_events :: JoinList (StartDelta, Event1)
                           }
  deriving (Eq,Show)

type instance DUnit EventList = Double

instance Monoid EventList where
  mempty = EventList { list_len = 0, list_events = mempty }
  EventList len0 se0 `mappend` EventList len1 se1 = 
      EventList (max len0 len1) (se0 `mappend` se1)

-- | maybe dtmax should be cached?
--
instance SReverse EventList where
  sreverse (EventList len se) = EventList len $ fmap fn se
    where
      dmax = F.foldr (\(dt,_) ac -> max ac dt) 0 se
      fn (dt,e) = (dmax - dt,e)

instance Scale EventList where
  scale sx (EventList len se) = EventList (len * sx) $ fmap fn se
    where
      fn (dt,e) = (dt * sx, scaleEvent1 sx e)

instance Translate EventList where
  translate dx (EventList len se) = EventList (len + dx) $ fmap fn se
    where
      fn (dt,e) = (dt + dx, e)



-- | Note @consec@ is not particularly efficient.
-- 
-- The right-hand list is iterated re-positioning each event.
--
consec :: EventList -> EventList -> EventList
consec (EventList len0 se0) (EventList len1 se1) = 
    EventList (len0 + len1) (se0 `mappend` fmap fn se1)
  where
    fn (dt,e) = (len0 + dt, e)




data Event1 = Instant MidiEvent
            | OnOff { _on_evt        :: MidiEvent
                    , _on_duration   :: Double 
                    , _off_event     :: MidiEvent
                    }
  deriving (Eq,Ord,Show)


type instance DUnit Event1 = Double



--------------------------------------------------------------------------------
-- Build


eventList :: [(StartDelta,Event1)] -> EventList
eventList ss = let (len,se) = foldr fn (0, JL.empty) ss in EventList len se
  where
    fn e@(dt,Instant {})    (len,ac) = (max dt len, JL.cons e ac) 
    fn e@(dt,OnOff _ drn _) (len,ac) = (max (dt+drn) len, JL.cons e ac)

eventList1 :: (StartDelta,Event1) -> EventList
eventList1 e@(dt,Instant {})    = EventList dt (JL.one e)
eventList1 e@(dt,OnOff _ drn _) = EventList (dt+drn) (JL.one e)


instant :: MidiEvent -> Event1
instant = Instant

onoff :: MidiEvent -> Double -> MidiEvent -> Event1
onoff = OnOff


instantEL :: StartDelta -> MidiVoiceEvent -> EventList
instantEL ot e = eventList1 (ot, instant $ VoiceEvent e)

onoffEL :: StartDelta -> MidiVoiceEvent -> Double -> MidiVoiceEvent -> EventList
onoffEL ot e0 drn e1 = 
    eventList1 (ot, onoff (VoiceEvent e0) drn (VoiceEvent e1))



--------------------------------------------------------------------------------
-- Extract 


durationEL :: EventList -> Double
durationEL = list_len

-- | Sorted plus trailing @end-of-track@ message.
--
extractMessages :: EventList -> [MidiMessage]
extractMessages = step 0 . sort . toListH . primMessages
  where  
    step ot ((t,e):xs) = let ut = durationr t 
                         in (fromIntegral $ ut-ot,e) : step ut xs
    step _  []         = [delta_end_of_track]


-- | Absolute time not delta time, unsorted...
--
primMessages :: EventList -> H (Double, MidiEvent)
primMessages (EventList _ se) = F.foldr fn emptyH se
  where
    fn (dt, Instant e)     ac = (dt, e) `consH` ac
    fn (dt, OnOff e0 d e1) ac = (dt, e0) `consH` (dt+d, e1) `consH` ac



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
-- Transform

scaleEvent1 :: Double -> Event1 -> Event1
scaleEvent1 _  (Instant e)     = Instant e
scaleEvent1 sx (OnOff e0 d e1) = OnOff e0 (d * sx) e1
