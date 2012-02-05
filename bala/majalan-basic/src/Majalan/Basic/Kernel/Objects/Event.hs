{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Basic.Kernel.Objects.Event
-- Copyright   :  (c) Stephen Tetley 2013
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- ...
--
--------------------------------------------------------------------------------

module Majalan.Basic.Kernel.Objects.Event
   (


     Event

   , runLocEvent
   , promoteLoc
   , moveStart

   , blank

   , decorate
   , elaborate
   , obliterate

   , space
   , sep

   )

   where



import Majalan.Basic.Kernel.Base.BaseDefs
import Majalan.Basic.Kernel.Base.RenderContext
import Majalan.Basic.Primitive.EventList


import Control.Applicative
import Data.Monoid



-- | 'Event' - function from onset time and Context to a 
-- polymorphic /answer/ and a duration and writer trace (CatEvent).
--
newtype Event ctx u a = Event { 
          getEvent :: RenderContext ctx -> OnsetTime -> (a, u, CatEvent) }


type instance UCtx (Event ctx u) = ctx

-- Functor

instance Functor (Event ctx u) where
  fmap f ma = Event $ \ctx loc -> 
                let (a,t1,w1) = getEvent ma ctx loc in (f a, t1, w1)


-- Applicative

instance (Ord u, InterpretUnit u) => Applicative (Event ctx u) where
  pure a    = Event $ \_   _   -> (a, 0, mempty)
  mf <*> ma = Event $ \ctx loc ->  
                let (f,d1,w1) = getEvent mf ctx loc
                    (a,d2,w2) = getEvent ma ctx loc
                in (f a, max d1 d2, w1 `mappend` w2)

-- Monad

instance (Ord u, InterpretUnit u) => Monad (Event ctx u) where
  return a = pure a
  m >>= k  = Event $ \ctx loc ->  
               let (a,d1,w1) = getEvent m ctx loc
                   (b,d2,w2) = getEvent (k a) ctx loc
               in (b, max d1 d2, w1 `mappend` w2)



-- Monoid

instance (Monoid a, Ord u, InterpretUnit u) => Monoid (Event ctx u a) where
  mempty          = pure mempty
  ma `mappend` mb = Event $ \ctx loc -> 
                        let (a,d1,w1) = getEvent ma ctx loc
                            (b,d2,w2) = getEvent mb ctx loc
                        in (a `mappend` b, max d1 d2, w1 `mappend` w2)


-- RenderContextM

instance (Ord u, InterpretUnit u) => RenderContextM (Event ctx u) where
  askCtx        = Event $ \ctx _   -> (ctx, 0 , mempty)
  localize f ma = Event $ \ctx loc -> getEvent ma (f ctx) loc



{-  


instance Decorate LocEvent where
  decorate ma mz = LocEvent $ \loc -> 
    decorate (getLocEvent ma loc) (getLocEvent mz loc)

  elaborate ma f = LocEvent $ \loc -> 
    elaborate (getLocEvent ma loc) (\a -> getLocEvent (f a) loc)

  obliterate ma mz = LocEvent $ \loc -> 
    getLocEvent ma ot `obliterate` getLocEvent mz ot

-- Note - if we want time unit @u@ representing symbolic durations 
-- (e.g. whole notes, quarter notes etc.) it makes sense to have 
-- tempo in the Context.
-- 
-}


runLocEvent :: InterpretUnit u
            => RenderContext ctx -> u -> Event ctx u a -> (a, u, CatEvent)
runLocEvent ctx loc ma = let bpm  = ctx_tempo ctx
                             dloc = normalize bpm loc
                         in getEvent ma ctx dloc


-- | A /non-event/ that lasts for the supplied duration.
--
blank :: InterpretUnit u => u -> Event ctx u ()
blank drn = Event $ \ctx loc -> 
    let bpm  = ctx_tempo ctx
        uloc = dinterp bpm loc
    in ((), uloc+drn, mempty)


promoteLoc ::  InterpretUnit u 
           => (RenderContext ctx -> u -> (a, u, CatEvent)) -> Event ctx u a
promoteLoc k = Event $ \ctx loc -> let bpm = ctx_tempo ctx
                                       uloc = dinterp bpm loc
                                   in k ctx uloc



-- 
-- The Decorate typeclass (vis Wumpus) does not seem very useful 
-- musically as we largely work with the idea of separate systems 
-- (tracks) played by a single instrument combined in a final 
-- score. Thus we don\'t want to decorate a synth note with a 
-- hi-hat - we want them coordinated but on separate systems.
--  
--
-- However there are potentially usefully monoidal combinators and
-- @bind@ like functions that @Decorate@ captures.
-- 
-- Contradiction - percussion may be more natural as multiple 
-- instruments in one system.
--


-- | Simultaneous start times...
--
decorate :: (Ord u, InterpretUnit u) 
         => Event ctx u a -> Event ctx u z -> Event ctx u a
decorate ma mz = ma >>= \a -> mz >> return a



-- | Note - 'obliterate' does not really merit a version with 
-- the arguments reversed. It should be simple enough to choose
-- the appropriate order in client code.
--
obliterate :: Ord u => Event ctx u a -> Event ctx u z -> Event ctx u a
obliterate ma mz = Event $ \ctx loc -> 
    let (a,d1,_)  = getEvent ma ctx loc
        (_,d2,w2) = getEvent mz ctx loc
    in (a, max d1 d2, w2)


-- | Should this work like a monadic @bind@ that also gives 
-- access to the duration of the first Event?
--
-- At the moment it works like decorate and drops the answer of 
-- the decoration.
--
elaborate :: (Ord u) 
          => Event ctx u a -> (a -> u -> Event ctx u z) -> Event ctx u a
elaborate ma f = Event $ \ctx loc -> 
    let (a,d1,w1) = getEvent ma ctx loc
        (_,d2,w2) = getEvent (f a d1) ctx loc
    in (a, max d1 d2, w1 `mappend` w2)
  




moveStart :: InterpretUnit u => (u -> u) -> Event ctx u a -> Event ctx u a
moveStart f ma = Event $ \ctx loc -> 
   let bpm  = ctx_tempo ctx
       uloc = f $ dinterp bpm loc
       dloc = normalize bpm uloc 
   in getEvent ma ctx dloc


{-

--------------------------------------------------------------------------------
-- Combining LocEvents 


--
-- LocEvents have no concept of /end-time/ as the are a functional 
-- type, so they can only be combined by manipulating the start 
-- time of successive events.
--



distrib :: (Monoid a, InterpretUnit u) 
        => u -> [LocEvent ctx u a]  -> LocEvent ctx u a
distrib _  []     = mempty
distrib dt (x:xs) = promoteLoc $ \ot -> 
    go (applyLoc x ot) (ot + dt) xs
  where
    go acc _  []     = acc
    go acc ot (a:as) = go (acc `mappend` applyLoc a ot) (ot + dt) as


-- | This is analogue to @replicate@ in the Prelude.
--
duplicate :: (Monoid a, InterpretUnit u) 
          => Int -> u -> LocEvent ctx u a -> LocEvent ctx u a
duplicate n _  _   | n < 1 = mempty
duplicate n dt evt         = go evt dt (n-1)
  where
     go acc _  i | i < 1 = acc
     go acc d1 i         = let evt1 = moveStart d1 evt
                           in go (acc `mappend` evt1) (d1 + dt) (i-1)

-}


space :: (Monoid a, Ord u, InterpretUnit u) 
      => u -> Event ctx u a -> Event ctx u a -> Event ctx u a
space d ma mb = elaborate ma fn `mappend` mb
  where
    fn _ drn = moveStart (drn+) $ blank d




sep :: (Monoid a, Ord u, InterpretUnit u) 
      => u -> [Event ctx u a] -> Event ctx u a
sep _ []     = mempty
sep d (x:xs) = go x xs
  where
    go ac []     = ac
    go ac (a:as) = go (space d ac a) as

