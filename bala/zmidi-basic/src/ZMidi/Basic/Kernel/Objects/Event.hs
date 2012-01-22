{-# LANGUAGE FlexibleInstances          #-}
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

    Event

  , runEvent

  , instant
  , onoff
  , blank

  , promoteLoc

  , ignoreAns
  , replaceAns

  , decorate
  , elaborate
  , obliterate
  , moveStart

  , space
  , sep

  ) where


import ZMidi.Basic.Kernel.Base.BaseDefs
import ZMidi.Basic.Kernel.Base.RenderContext

import ZMidi.Basic.Primitive.EventList ( CatEvent )
import qualified ZMidi.Basic.Primitive.EventList as E

import ZMidi.Core                               -- package: zmidi-core

import Control.Applicative
import Data.Monoid
import Data.Word




-- | Return answer @a@ plus duration and writer trace.
--
newtype Event u a = Event { 
          getEvent :: RenderContext -> OnsetTime -> (a, u, CatEvent) }


-- Functor

instance Functor (Event u) where
  fmap f mf = Event $ \ctx loc -> 
              let (a,t1,w1) = getEvent mf ctx loc in (f a,t1,w1)


-- Applicative

instance (Ord u, InterpretUnit u) => Applicative (Event u) where
  pure a    = Event $ \_   _   -> (a , 0, mempty)
  mf <*> ma = Event $ \ctx loc -> let (f,d1,w1) = getEvent mf ctx loc
                                      (a,d2,w2) = getEvent ma ctx loc
                                  in (f a, max d1 d2, w1 `mappend` w2)


-- Monad

instance (Ord u, InterpretUnit u) => Monad (Event u) where
  return  = pure
  m >>= k = Event $ \ctx loc -> let (a,d1,w1) = getEvent m ctx loc
                                    (b,d2,w2) = getEvent (k a) ctx loc
                                in (b, max d1 d2, w1 `mappend` w2)


-- Monoid

instance (Monoid a, Ord u, InterpretUnit u) => Monoid (Event u a) where
  mempty          = pure mempty
  ma `mappend` mb = Event $ \ctx loc -> 
                        let (a,d1,w1) = getEvent ma ctx loc
                            (b,d2,w2) = getEvent mb ctx loc
                        in (a `mappend` b, max d1 d2, w1 `mappend` w2)
                     

instance (Ord u, InterpretUnit u) => RenderContextM (Event u) where
  askCtx        = Event $ \ctx _ -> (ctx, 0, mempty)
  localize f ma = Event $ \ctx loc -> getEvent ma (f ctx) loc




--------------------------------------------------------------------------------

--
-- NOTE - having a modulating BPM in the RenderContext makes for 
-- foggy semantics in the @runEvent@ function.
-- 
-- Modulating BPM may have modulated to something else by the end 
-- time of the TimeSpan.
-- 


runEvent :: InterpretUnit u 
         => RenderContext -> u -> Event u a -> (a, u, CatEvent)
runEvent ctx loc ma = let bpm = interp_bpm ctx
                          dloc = normalize bpm loc
                      in getEvent ma ctx dloc

                          
instant :: InterpretUnit u => (Word8 -> MidiVoiceEvent) -> Event u ()
instant fn = Event $ \ctx loc -> let chan = rc_channel_num ctx
                                     bpm  = interp_bpm ctx
                                     uloc = dinterp bpm loc
                                     e    = VoiceEvent $ fn chan
                                 in ((), uloc, E.instant loc e)

onoff :: InterpretUnit u 
      => (Word8 -> MidiVoiceEvent) -> (Word8 -> MidiVoiceEvent) -> u 
      -> Event u ()
onoff onf offf drn = Event $ \ctx loc -> 
    let chan = rc_channel_num ctx
        bpm  = interp_bpm ctx
        ddrn = normalize bpm drn
        uloc = dinterp bpm loc
        e0   = VoiceEvent $ onf chan
        e1   = VoiceEvent $ offf chan
    in ((), uloc+drn, E.onoff loc e0 ddrn e1)




-- | A /non-event/ that lasts for the supplied duration.
--
blank :: InterpretUnit u => u -> Event u ()
blank drn = Event $ \ctx loc -> 
    let bpm  = interp_bpm ctx
        uloc = dinterp bpm loc
    in ((), uloc+drn, mempty)



promoteLoc ::  InterpretUnit u 
           => (RenderContext -> u -> (a, u, CatEvent)) -> Event u a
promoteLoc k = Event $ \ctx loc -> let bpm = interp_bpm ctx
                                       uloc = dinterp bpm loc
                                   in k ctx uloc


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
          => Event u a -> (a -> u -> Event u z) -> Event u a
elaborate ma f = Event $ \ctx loc -> 
    let (a,d1,w1) = getEvent ma ctx loc
        (_,d2,w2) = getEvent (f a d1) ctx loc
    in (a, max d1 d2, w1 `mappend` w2)
  

obliterate :: Ord u => Event u a -> Event u z -> Event u a
obliterate ma mz = Event $ \ctx loc -> 
    let (a,d1,_)  = getEvent ma ctx loc
        (_,d2,w2) = getEvent mz ctx loc
    in (a, max d1 d2, w2)

moveStart :: InterpretUnit u => (u -> u) -> Event u a -> Event u a
moveStart f ma = Event $ \ctx loc -> 
   let bpm  = interp_bpm ctx
       uloc = f $ dinterp bpm loc
       dloc = normalize bpm uloc 
   in getEvent ma ctx dloc


{-
concatE :: (Monoid a, Ord u, InterpretUnit u) 
       => Event u a -> Event u a -> Event u a
concatE ma mb = Event $ \ctx loc -> 
    let (a,d1,w1) = getEvent ma ctx loc
        (b,d2,w2) = getEvent mb ctx loc
    in (a `mappend` b, max d1 d2, w1 `mappend` w2)
-}  



space :: (Monoid a, Ord u, InterpretUnit u) 
      => u -> Event u a -> Event u a -> Event u a
space d ma mb = elaborate ma fn `mappend` mb
  where
    fn _ drn = moveStart (drn+) $ blank d

sep :: (Monoid a, Ord u, InterpretUnit u) 
      => u -> [Event u a] -> Event u a
sep _ []     = mempty
sep d (x:xs) = go x xs
  where
    go ac []     = ac
    go ac (a:as) = go (space d ac a) as
