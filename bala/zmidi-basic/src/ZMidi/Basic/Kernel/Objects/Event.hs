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
  , DTimeSpan
  , instantTimeSpan
  , unionTimeSpan
  , durationTimeSpan

  , normalizeTimeSpan
  , dinterpTimeSpan

  , Event

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

  , (><)
  , space
  , sep

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

type DTimeSpan = TimeSpan Double

instantTimeSpan :: u -> TimeSpan u
instantTimeSpan t0 = TimeSpan { start_pos = t0, end_pos = t0 }

unionTimeSpan :: Ord u => TimeSpan u -> TimeSpan u -> TimeSpan u
unionTimeSpan (TimeSpan s0 e0) (TimeSpan s1 e1) = 
    TimeSpan { start_pos = min s0 s1, end_pos = max e0 e1 }

-- Can events always be /from Time to/ - do we need a saturated
-- event (already applied to time)?

durationTimeSpan :: Num u => TimeSpan u -> u
durationTimeSpan (TimeSpan s e) = e - s

normalizeTimeSpan :: InterpretUnit u => BPM -> TimeSpan u -> DTimeSpan
normalizeTimeSpan bpm (TimeSpan s e) = 
    TimeSpan (normalize bpm s) (normalize bpm e)


dinterpTimeSpan :: InterpretUnit u => BPM -> DTimeSpan -> TimeSpan u
dinterpTimeSpan bpm (TimeSpan s e) = TimeSpan (dinterp bpm s) (dinterp bpm e)




-- | Should the returned TimeSpan be in user units?
--
newtype Event u a = Event { 
          getEvent :: RenderContext -> OnsetTime -> (a, TimeSpan u, CatPrim) }


-- Functor

instance Functor (Event u) where
  fmap f mf = Event $ \ctx loc -> 
              let (a,t1,w1) = getEvent mf ctx loc in (f a,t1,w1)


-- Applicative

instance (Ord u, InterpretUnit u) => Applicative (Event u) where
  pure a    = Event $ \ctx loc -> let bpm  = interp_bpm ctx
                                      uloc = dinterp bpm loc
                                  in (a , instantTimeSpan uloc, mempty)

  mf <*> ma = Event $ \ctx loc -> let (f,s1,w1) = getEvent mf ctx loc
                                      (a,s2,w2) = getEvent ma ctx loc
                                  in (f a, unionTimeSpan s1 s2, w1 `mappend` w2)


-- Monad

instance (Ord u, InterpretUnit u) => Monad (Event u) where
  return  = pure
  m >>= k = Event $ \ctx loc -> let (a,s1,w1) = getEvent m ctx loc
                                    (b,s2,w2) = getEvent (k a) ctx loc
                                in (b, unionTimeSpan s1 s2, w1 `mappend` w2)


-- Monoid

instance (Monoid a, Ord u, InterpretUnit u) => Monoid (Event u a) where
  mempty          = pure mempty
  ma `mappend` mb = Event $ \ctx loc -> 
                        let (a,s1,w1) = getEvent ma ctx loc
                            (b,s2,w2) = getEvent mb ctx loc
                        in (a `mappend` b, unionTimeSpan s1 s2, w1 `mappend` w2)
                     

instance (Ord u, InterpretUnit u) => RenderContextM (Event u) where
  askCtx = Event $ \ctx loc -> let bpm  = interp_bpm ctx
                                   uloc = dinterp bpm loc
                               in (ctx , instantTimeSpan uloc, mempty)

  localize f ma = Event $ \ctx loc -> getEvent ma (f ctx) loc


--
-- NOTE - having a modulating BPM in the RenderContext makes for 
-- foggy semantics in the @runEvent@ function.
-- 
-- Modulating BPM may have modulated to something else by the end 
-- time of the TimeSpan.
-- 


runEvent :: InterpretUnit u 
         => RenderContext -> u -> Event u a -> (a, TimeSpan u, CatPrim)
runEvent ctx loc ma = let bpm = interp_bpm ctx
                          dloc = normalize bpm loc
                      in getEvent ma ctx dloc
                          
instant :: InterpretUnit u => (Word8 -> MidiVoiceEvent) -> Event u ()
instant fn = Event $ \ctx loc -> let chan = rc_channel_num ctx
                                     bpm  = interp_bpm ctx
                                     uloc = dinterp bpm loc
                                 in ((), instantTimeSpan uloc, prim1 (loc, fn chan))

onoff :: InterpretUnit u 
      => (Word8 -> MidiVoiceEvent) -> (Word8 -> MidiVoiceEvent) -> u 
      -> Event u ()
onoff onf offf drn = Event $ \ctx loc -> 
    let chan = rc_channel_num ctx
        bpm  = interp_bpm ctx
        ddrn = normalize bpm drn
        uloc = dinterp bpm loc
        w1   = prim1 (loc, onf chan)
        w2   = prim1 (loc + ddrn, offf chan)
    in ((), TimeSpan uloc (uloc+drn), w1 `mappend` w2)




-- | A /non-event/ that lasts for the supplied duration.
--
blank :: InterpretUnit u => u -> Event u ()
blank drn = Event $ \ctx loc -> 
    let bpm  = interp_bpm ctx
        uloc = dinterp bpm loc
    in ((), TimeSpan uloc (uloc+drn), mempty)



promoteLoc ::  InterpretUnit u 
           => (RenderContext -> u -> (a, TimeSpan u, CatPrim)) -> Event u a
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
          => Event u a -> (a -> TimeSpan u -> Event u z) -> Event u a
elaborate ma f = Event $ \ctx loc -> 
    let (a,s1,w1) = getEvent ma ctx loc
        (_,s2,w2) = getEvent (f a s1) ctx loc
    in (a, unionTimeSpan s1 s2, w1 `mappend` w2)
  

obliterate :: Ord u => Event u a -> Event u z -> Event u a
obliterate ma mz = Event $ \ctx loc -> 
    let (a,s1,_)  = getEvent ma ctx loc
        (_,s2,w2) = getEvent mz ctx loc
    in (a, unionTimeSpan s1 s2, w2)

moveStart :: InterpretUnit u => (u -> u) -> Event u a -> Event u a
moveStart f ma = Event $ \ctx loc -> 
   let bpm  = interp_bpm ctx
       uloc = f $ dinterp bpm loc
       dloc = normalize bpm uloc 
   in getEvent ma ctx dloc

concatE :: (Monoid a, Ord u, InterpretUnit u) 
       => Event u a -> Event u a -> Event u a
concatE ma mb = Event $ \ctx loc -> 
    let bpm       = interp_bpm ctx
        (a,s1,w1) = getEvent ma ctx loc
        len1      = normalize bpm $ durationTimeSpan s1
        (b,s2,w2) = getEvent mb ctx (loc + len1)
    in (a `mappend` b, unionTimeSpan s1 s2, w1 `mappend` w2)
  
infixr 5 ><

-- | @concat@
--
-- > infixr 5 ><
--
(><) :: (Monoid a, Ord u, InterpretUnit u) 
     => Event u a -> Event u a -> Event u a
(><) = concatE



space :: (Monoid a, Ord u, InterpretUnit u) 
      => u -> Event u a -> Event u a -> Event u a
space d ma mb = elaborate ma fn >< mb
  where
    fn _ spn = moveStart ((+) $ durationTimeSpan spn) $ blank d

sep :: (Monoid a, Ord u, InterpretUnit u) 
      => u -> [Event u a] -> Event u a
sep _ []     = mempty
sep d (x:xs) = go x xs
  where
    go ac []     = ac
    go ac (a:as) = go (space d ac a) as
