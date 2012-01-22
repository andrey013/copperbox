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
  , CatEvent

  , instant
  , onoff
  , eventList

  , duration
  , durationCE 

  , toList
  , overlay

  , stretch
  , reverse
  , delay  

  ) where


import ZMidi.Basic.Utils.HList

import ZMidi.Core                               -- package: zmidi-core


import Data.List ( sort )
import Data.Monoid
import Data.Word
import Prelude hiding ( reverse )



type Onset = Double


-- | EventList datatype - internally this is a binary tree of
-- lists. The binary tree enables efficient concatenation during
-- building. Duration of the @EventList@ is cached.
--
data EventList = 
      One { init_delay  :: !Double
          , list_dur    :: !Double
          , leaf_list   :: [(Onset, Event1)]
          }
    | Cat { init_delay  :: !Double
          , list_dur    :: !Double
          , _list_left  :: EventList
          , _list_right :: EventList
          }
  deriving (Eq,Ord,Show)


data Event1 = 
      Instant MidiEvent
    | OnOff { on_event      :: MidiEvent
            , on_duration   :: Double 
            , off_event     :: MidiEvent
            }
  deriving (Eq,Ord,Show)



instance Monoid EventList where
  mempty = One 0 0 []
  a `mappend` b = let b' = delay (list_dur a) b in Cat 0 (list_dur b') a b'



-- | Although concat on EventLists is operationally efficient
-- (just a constructor call, no traversing until a transform or 
-- the final rendering), it would produce an 
-- /inefficiently shaped/ tree if it were used for all 
-- intermediate events - we really need to be able to build good 
-- lists for the @leaf_list@ of the One constructor. 
-- 
-- We have a second data type to support building leaf lists 
-- efficiently. 
--
data CatEvent = CatEvent 
      { cat_dur :: !Double
      , cat_events :: H (Onset,Event1)
      }
                       
instance Show CatEvent where
  show _ = "<< CatEvent >>"


instance Monoid CatEvent where
  mempty = CatEvent { cat_dur = 0, cat_events = emptyH }

  CatEvent d0 hs0 `mappend` CatEvent d1 hs1 = 
    CatEvent (max d0 d1) (appendH hs0 hs1) 

durationCE :: CatEvent -> Double
durationCE = cat_dur

--------------------------------------------------------------------------------
-- Build



instant :: Onset -> MidiEvent -> CatEvent
instant ot e = CatEvent ot $ wrapH (ot,Instant e)

onoff :: Onset -> MidiEvent -> Double -> MidiEvent -> CatEvent
onoff ot note_on d note_off = CatEvent (ot+d) $ wrapH (ot,evt)
  where
    evt = OnOff { on_event    = note_on
                , on_duration = d
                , off_event   = note_off
                }

eventList :: CatEvent -> EventList
eventList c = One { init_delay  = 0
                  , list_dur    = cat_dur c
                  , leaf_list   = toListH $ cat_events c 
                  }


--------------------------------------------------------------------------------


duration :: EventList -> Double 
duration = list_dur

-- | Needs a better name...
--
overlay :: EventList -> EventList -> EventList
overlay a b = Cat 0 max_len a b
  where
    max_len = max (list_dur a) (list_dur b)


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





--------------------------------------------------------------------------------
-- Transform


-- | @stretch@ also stretches the initial onset if there is one.
--
stretch :: Double -> EventList -> EventList
stretch sx es = One { init_delay  = sx * init_delay es
                    , list_dur    = sx * list_dur es
                    , leaf_list   = toListH $ outer 0 es }
  where
    outer dt (One _ d1 xs)  = inner (dt+d1) xs
    outer dt (Cat _ d1 l r) = outer (dt+d1) l `appendH` outer (dt+d1) r

    inner _  []         = emptyH
    inner dt ((t,e):xs) = consH (sx * (dt+t), stretchEvent1 sx e) $ inner dt xs




stretchEvent1 :: Double -> Event1 -> Event1
stretchEvent1 _  (Instant e)     = Instant e
stretchEvent1 sx (OnOff e0 d e1) = OnOff e0 (d * sx) e1


-- NOT WORKING ....

-- | We do not explicitly have a trailing delay, however 
-- preserving the list duration means that any subsequent 
-- concatenation we be placed at the end of the list 
-- even if the list ends in silence.
--
reverse :: EventList -> EventList
reverse es = One { init_delay  = 0
                 , list_dur    = tot
                 , leaf_list   = toListH $ outer 0 es }
  where
    tot                      = list_dur es
    outer dly (One d1 _ xs)  = inner (dly+d1) xs
    outer dly (Cat d1 _ l r) = outer (dly+d1) l `appendH` outer (dly+d1) r

    inner _   []          = emptyH
    inner dly ((ot,e):xs) = let d = durationE1 e 
                            in consH (tot - (dly+ot+d),e) $ inner dly xs

durationE1 :: Event1 -> Double
durationE1 (Instant _)   = 0
durationE1 (OnOff _ d _) = d






-- sreverse 
-- sreverse [(10,A), (12,B)] == [(0,B), (2,A)]

-- stretch 
-- stretch 0.5 [(10,A), (12,B)] == [(5,A), (6,B)]

-- delay 
-- delay 5 [(10,A), (12,B)] == [(15,A), (17,B)]

-- ovarlay 
-- [(10,A), (12,B)] `mappend`  [(5,C)] == [(5,C), (10,A), (12,B)]

-- mappend
-- [(10,A), (12,B)] `consec`  [(5,C)] == [(10,A), (12,B), (17,C)]



