{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Basic.Primitive.EventList
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- EventList representation.
--
--------------------------------------------------------------------------------

module ZMidi.Basic.Primitive.EventList
  ( 
   
    EventList

  , toList
  , delay  
  , parallel

  ) where


import ZMidi.Basic.Utils.HList

import ZMidi.Core                               -- package: zmidi-core


-- import qualified Data.Foldable as F
import Data.List ( sort )
import Data.Monoid
import Data.Word


type Onset = Double

data EventList = 
      One { init_delay  :: !Double
          , list_len    :: !Double
          , one_body    :: [(Onset, Event1)]
          }
    | Cat { init_delay  :: !Double
          , list_len    :: !Double
          , list_left   :: EventList
          , list_right  :: EventList
          }
  deriving (Eq,Ord,Show)


data Event1 = 
      Instant MidiEvent
    | OnOff { on_evt        :: MidiEvent
            , on_duration   :: Double 
            , off_event     :: MidiEvent
            }
  deriving (Eq,Ord,Show)



instance Monoid EventList where
  mempty = One 0 0 []
  a `mappend` b = let b' = delay (list_len a) b in Cat 0 (list_len b') a b'


-- | Needs a better name...
--
parallel :: EventList -> EventList -> EventList
parallel a b = Cat 0 max_len a b
  where
    max_len = max (list_len a) (list_len b)


-- | Delay time should be positive!
-- 
-- Internally @delay@ does not rebuild the list.
--
delay :: Double -> EventList -> EventList
delay dx (One d len xs)  = One (d+dx) (len+dx) xs
delay dx (Cat d len l r) = Cat (d+dx) (len+dx) l r



-- | Sorted and delta time transformed.
--
toList :: EventList -> [MidiMessage] 
toList = delta 0 . sort . toListH . outer 0 
  where
    outer dt (One d _ xs)   = inner (d+dt) xs
    outer dt (Cat d _ l r)  = outer (dt+d) l `appendH` outer (dt+d) r

    inner _  []                       = emptyH
    inner dt ((ot, Instant e):xs)     = (dt+ot,e) `consH` inner dt xs
    inner dt ((ot, OnOff e0 d e1):xs) = let a = (dt+ot,e0) 
                                            b = (dt+ot+d,e1)
                                        in a `consH` b `consH` inner dt xs

    delta ot ((t,e):xs) = let ut = durationr t 
                          in (fromIntegral $ ut-ot,e) : delta ut xs
    delta _  []         = [delta_end_of_track]



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




{-





-- sreverse 
-- sreverse [(10,A), (12,B)] == [(0,B), (2,A)]

-- stretch 
-- stretch 0.5 [(10,A), (12,B)] == [(5,A), (6,B)]

-- delay 
-- delay 5 [(10,A), (12,B)] == [(15,A), (17,B)]

-- mappend 
-- [(10,A), (12,B)] `mappend`  [(5,C)] == [(5,C), (10,A), (12,B)]

-- consecutive
-- [(10,A), (12,B)] `consec`  [(5,C)] == [(10,A), (12,B), (17,C)]


-- | The @list_events@ field is not necessarily ordered and the 
-- onsets are distance from the /list/ start to the event, 
-- \*\* they are not \*\* deltas between consecutive events.
--
-- This representation is rather naive but it does allow simple
-- (if inefficient) scaling, reversal and translation.
-- 
data EventList = EventList { list_len    :: Double
                           , list_events :: JoinList (Onset, Event1)
                           }
  deriving (Eq,Show)

type instance DUnit EventList = Double

instance Monoid EventList where
  mempty = EventList { list_len = 0, list_events = mempty }
  EventList len0 se0 `mappend` EventList len1 se1 = 
      EventList (max len0 len1) (se0 `mappend` se1)


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





type instance DUnit Event1 = Double



--------------------------------------------------------------------------------
-- Build


eventList :: [(Onset,Event1)] -> EventList
eventList ss = let (len,se) = foldr fn (0, JL.empty) ss in EventList len se
  where
    fn e@(dt,Instant {})    (len,ac) = (max dt len, JL.cons e ac) 
    fn e@(dt,OnOff _ drn _) (len,ac) = (max (dt+drn) len, JL.cons e ac)

eventList1 :: (Onset,Event1) -> EventList
eventList1 e@(dt,Instant {})    = EventList dt (JL.one e)
eventList1 e@(dt,OnOff _ drn _) = EventList (dt+drn) (JL.one e)


instant :: MidiEvent -> Event1
instant = Instant

onoff :: MidiEvent -> Double -> MidiEvent -> Event1
onoff = OnOff


instantEL :: Onset -> MidiVoiceEvent -> EventList
instantEL ot e = eventList1 (ot, instant $ VoiceEvent e)

onoffEL :: Onset -> MidiVoiceEvent -> Double -> MidiVoiceEvent -> EventList
onoffEL ot e0 drn e1 = 
    eventList1 (ot, onoff (VoiceEvent e0) drn (VoiceEvent e1))


--------------------------------------------------------------------------------
-- Transform

scaleEvent1 :: Double -> Event1 -> Event1
scaleEvent1 _  (Instant e)     = Instant e
scaleEvent1 sx (OnOff e0 d e1) = OnOff e0 (d * sx) e

-}

