{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Basic.Primitive.EventList
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

module Majalan.Basic.Primitive.EventList
  ( 
   
    EventList
  , CatEvent

  , event
  , eventList

  , duration
  , durationEvt
  , onsetEvt

  , toList
  , overlay

  , stretch
  , reverse
  , delay  

  ) where


import Majalan.Basic.Utils.HList

import Majalan.Core                             -- package: majalan-core


import Data.List ( sort )
import Data.Monoid
import Prelude hiding ( reverse )



type Onset = Double

--
-- Note - 02 Feb 2012
--
-- There is a good case for functorial notelists, but functorial
-- map will not be able to access onset or duration (as this 
-- could invalidate the cached measurements).
--
-- Thus the prototype event would typically be a function from 
-- onset and duration to CsEvent.
--

-- | EventList datatype - internally this is a binary tree of
-- lists. The binary tree enables efficient concatenation during
-- building. Duration of the @EventList@ is cached.
--
data EventList = 
      One { init_delay  :: !Double
          , list_dur    :: !Double
          , leaf_list   :: [CsEvent]
          }
    | Cat { init_delay  :: !Double
          , list_dur    :: !Double
          , _list_left  :: EventList
          , _list_right :: EventList
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
      , cat_events :: H CsEvent
      }
                       
instance Show CatEvent where
  show _ = "<< CatEvent >>"


instance Monoid CatEvent where
  mempty = CatEvent { cat_dur = 0, cat_events = emptyH }

  CatEvent d0 hs0 `mappend` CatEvent d1 hs1 = 
    CatEvent (max d0 d1) (appendH hs0 hs1) 

durationEvt :: CsEvent -> Double
durationEvt = event_duration

onsetEvt :: CsEvent -> Double
onsetEvt = event_onset

setDuration :: Double -> CsEvent -> CsEvent 
setDuration d e = e { event_duration = d }

setOnset :: Double -> CsEvent -> CsEvent 
setOnset ot e = e { event_onset = ot }

updOnset :: (Double -> Double) -> CsEvent -> CsEvent
updOnset f e = let ot = event_onset e in e { event_onset = f ot }

--------------------------------------------------------------------------------
-- Build




event :: Int -> Onset -> Double -> [CsValue] -> CatEvent
event inum ot dn args = CatEvent (ot+dn) $ wrapH evt
  where
    evt = CsEvent { instr_num       = inum
                  , event_onset     = ot
                  , event_duration  = dn 
                  , event_args      = args
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




-- | Sorted by onset time.
--
toList :: EventList -> [CsEvent] 
toList = sort . toListH . outer 0 
  where
    outer dx (One d _ xs)   = inner (dx+d) xs
    outer dx (Cat d _ l r)  = outer (dx+d) l `appendH` outer (dx+d) r

    inner _  []         = emptyH
    inner dx (e:es)     = updOnset (dx+) e `consH` inner dx es



--------------------------------------------------------------------------------
-- Transform


-- | @stretch@ also stretches the initial onset if there is one.
--
stretch :: Double -> EventList -> EventList
stretch sx evts = One { init_delay  = sx * init_delay evts
                      , list_dur    = sx * list_dur evts
                      , leaf_list   = toListH $ outer 0 evts }
  where
    outer dt (One _ d1 xs)  = inner (dt+d1) xs
    outer dt (Cat _ d1 l r) = outer (dt+d1) l `appendH` outer (dt+d1) r

    inner _  []     = emptyH
    inner dt (e:es) = let ot = onsetEvt e
                          e1 = setOnset (sx * (ot+dt)) $ stretchEvt sx e
                      in consH e1 $ inner dt es




stretchEvt :: Double -> CsEvent -> CsEvent
stretchEvt sx e = let d = durationEvt e in setDuration (d * sx) e


-- | We do not explicitly have a trailing delay, however 
-- preserving the list duration means that any subsequent 
-- concatenation we be placed at the end of the list 
-- even if the list ends in silence.
--
reverse :: EventList -> EventList
reverse evts = One { init_delay  = 0
                   , list_dur    = tot
                   , leaf_list   = toListH $ outer 0 evts }
  where
    tot                      = list_dur evts
    outer dly (One d1 _ xs)  = inner (dly+d1) xs
    outer dly (Cat d1 _ l r) = outer (dly+d1) l `appendH` outer (dly+d1) r

    inner _   []          = emptyH
    inner dly (e:es)      = let d  = durationEvt e
                                ot = onsetEvt e
                                e1 = setDuration (tot - (dly+ot+d)) e
                            in consH e1 $ inner dly es







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



