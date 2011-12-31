{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Basic.Kernel.Objects.Event
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Event...
--
--------------------------------------------------------------------------------

module ZMidi.Basic.Kernel.Objects.Event
  ( 
   
    TimeSpan(..)
  , Event

  , runEvent

  , instant
  , onoff

  , ignoreAns
  , replaceAns

  , decorate
  , elaborate
  , obliterate
  , moveStart

  , (><)

  ) where


import ZMidi.Basic.Kernel.Base.BaseDefs
import ZMidi.Basic.Kernel.Base.RenderContext
import ZMidi.Basic.Kernel.Base.WrappedPrimitive

import ZMidi.Core                               -- package: zmidi-core

import Control.Applicative
import Data.Monoid
import Data.Word

-- | TimeSpan cf. a bounding box.
--
data TimeSpan u = TimeSpan { start_pos :: u, end_pos :: u }


instantTS :: u -> TimeSpan u
instantTS t0 = TimeSpan { start_pos = t0, end_pos = t0 }

unionTS :: Ord u => TimeSpan u -> TimeSpan u -> TimeSpan u
unionTS (TimeSpan s0 e0) (TimeSpan s1 e1) = 
    TimeSpan { start_pos = min s0 s1, end_pos = max e0 e1 }

-- Can events always be /from Time to/ - do we need a saturated
-- event (already applied to time)?

durationTS :: Num u => TimeSpan u -> u
durationTS (TimeSpan s e) = e - s

-- | Should the returned TimeSpan be in user units?
--
newtype Event u a = Event { 
          getEvent :: OnsetTime -> RenderContext -> (a, TimeSpan u, CatPrim) }


-- Functor

instance Functor (Event u) where
  fmap f mf = Event $ \loc ctx -> 
              let (a,t1,w1) = getEvent mf loc ctx in (f a,t1,w1)


-- Applicative

instance (Ord u, InterpretUnit u) => Applicative (Event u) where
  pure a    = Event $ \loc ctx -> let bpm  = interp_bpm ctx
                                      uloc = dinterp bpm loc
                                  in (a , instantTS uloc, mempty)
  mf <*> ma = Event $ \loc ctx -> let (f,s1,w1) = getEvent mf loc ctx
                                      (a,s2,w2) = getEvent ma loc ctx
                                  in (f a, unionTS s1 s2, w1 `mappend` w2)


-- Monad

instance (Ord u, InterpretUnit u) => Monad (Event u) where
  return  = pure
  m >>= k = Event $ \loc ctx -> let (a,s1,w1) = getEvent m loc ctx
                                    (b,s2,w2) = getEvent (k a) loc ctx
                                in (b, unionTS s1 s2, w1 `mappend` w2)


-- Monoid

instance (Monoid a, Ord u, InterpretUnit u) => Monoid (Event u a) where
  mempty          = pure mempty
  ma `mappend` mb = Event $ \loc ctx -> 
                        let (a,s1,w1) = getEvent ma loc ctx
                            (b,s2,w2) = getEvent mb loc ctx
                        in (a `mappend` b, unionTS s1 s2, w1 `mappend` w2)
                     

instance (Ord u, InterpretUnit u) => RenderCtxM (Event u) where
  askCtx = Event $ \loc ctx -> let bpm  = interp_bpm ctx
                                   uloc = dinterp bpm loc
                               in (ctx , instantTS uloc, mempty)
  localize f ma = Event $ \loc ctx -> getEvent ma loc (f ctx)


--
-- NOTE - having a modulating BPM in the RenderContext makes for 
-- foggy semantics in the @runEvent@ function.
-- 
-- Modulating BPM may have modulated to something else by the end 
-- time of the TimeSpan.
-- 


runEvent :: InterpretUnit u 
         => u -> RenderContext -> Event u a -> (a, TimeSpan u, CatPrim)
runEvent loc ctx ma = let bpm = interp_bpm ctx
                          dloc = normalize bpm loc
                      in getEvent ma dloc ctx
                          
instant :: InterpretUnit u => (Word8 -> MidiVoiceEvent) -> Event u ()
instant fn = Event $ \loc ctx -> let chan = rc_channel_num ctx
                                     bpm  = interp_bpm ctx
                                     uloc = dinterp bpm loc
                                 in ((), instantTS uloc, prim1 (loc, fn chan))

onoff :: InterpretUnit u 
      => (Word8 -> MidiVoiceEvent) -> (Word8 -> MidiVoiceEvent) -> u 
      -> Event u ()
onoff onf offf drn = Event $ \loc ctx -> 
    let chan = rc_channel_num ctx
        bpm  = interp_bpm ctx
        ddrn = normalize bpm drn
        uloc = dinterp bpm loc
        w1   = prim1 (loc, onf chan)
        w2   = prim1 (loc + ddrn, offf chan)
        in ((), TimeSpan uloc (uloc+drn), w1 `mappend` w2)


ignoreAns :: Event u a -> Event u ()
ignoreAns = fmap (const ())

-- | Replace the answer produced by a graphic object.
--
replaceAns :: a -> Event u z -> Event u a
replaceAns a = fmap (const a)



-- | Simultaneous start times...
--
decorate :: (Ord u, InterpretUnit u) 
         => Event u a -> Event u z -> Event u a
decorate ma mz = ma >>= \a -> mz >> return a


elaborate :: (Ord u) 
          => Event u a -> (a -> TimeSpan u -> Event u z) -> Event u a
elaborate ma f = Event $ \loc ctx -> 
    let (a,s1,w1) = getEvent ma loc ctx
        (_,s2,w2) = getEvent (f a s1) loc ctx
    in (a, unionTS s1 s2, w1 `mappend` w2)
  

obliterate :: Ord u => Event u a -> Event u z -> Event u a
obliterate ma mz = Event $ \loc ctx -> 
    let (a,s1,_)  = getEvent ma loc ctx
        (_,s2,w2) = getEvent mz loc ctx
    in (a, unionTS s1 s2, w2)

moveStart :: InterpretUnit u => (u -> u) -> Event u a -> Event u a
moveStart f ma = Event $ \loc ctx -> 
   let bpm  = interp_bpm ctx
       uloc = f $ dinterp bpm loc
       dloc = normalize bpm uloc 
   in getEvent ma dloc ctx

concatE :: (Monoid a, Ord u, InterpretUnit u) 
       => Event u a -> Event u a -> Event u a
concatE ma mb = Event $ \loc ctx -> 
    let bpm       = interp_bpm ctx
        (a,s1,w1) = getEvent ma loc ctx
        len1      = normalize bpm $ durationTS s1
        (b,s2,w2) = getEvent mb (loc + len1) ctx
    in (a `mappend` b, unionTS s1 s2, w1 `mappend` w2)
  
infixr 5 ><

-- | @concat@
--
-- > infixr 5 ><
--
(><) :: (Monoid a, Ord u, InterpretUnit u) 
     => Event u a -> Event u a -> Event u a
(><) = concatE