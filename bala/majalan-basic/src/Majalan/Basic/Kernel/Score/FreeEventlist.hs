{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Basic.Kernel.Score.FreeEventlist
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Generate a event list. Events can be freely placed on the 
-- timeline, they do not have to be sequential (the timeline is 
-- sorted before rendering).
-- 
--------------------------------------------------------------------------------

module Majalan.Basic.Kernel.Score.FreeEventlist
  (

    renderEventlist
  , renderEventlistU  

  -- * Collect primitives (output class) 
  , TraceM(..)

  -- * Event list
  , Eventlist
  , DEventlist
  , EventlistT
  , DEventlistT

  , runEventlist
  , execEventlist
  , evalEventlist

  , runEventlistT
  , execEventlistT
  , evalEventlistT

  , event
  , eventi
  , eventl
  , eventli


  ) where


import Majalan.Basic.Kernel.Base.BaseDefs
import Majalan.Basic.Kernel.Base.Context
import Majalan.Basic.Kernel.Base.WrappedPrimitive
import Majalan.Basic.Kernel.Objects.Basis
import Majalan.Basic.Kernel.Objects.LocEvent

import Majalan.Core                             -- package: majalan-core

import Control.Applicative
import Control.Monad
import Data.Monoid


--------------------------------------------------------------------------------



renderEventlist :: Context ctx -> Eventlist ctx u a -> Maybe RScore
renderEventlist ctx mf = hprimToScoreMb $ execEventlist ctx mf


renderEventlistU :: Context ctx -> Eventlist ctx u a -> RScore
renderEventlistU ctx mf = maybe fk id $ renderEventlist ctx mf
  where
    fk = error "renderEventlistU - empty score." 






-- | Collect elementary events as part of a larger score.
--
-- TraceM works much like a writer monad.
--
class TraceM (m :: * -> *) where
  trace     :: (DUnit (m ()) ~ u) => HPrim u -> m ()




newtype Eventlist ctx u a   = Eventlist { 
          getEventlist :: Context ctx -> (a, HPrim u) }

newtype EventlistT ctx u m a = EventlistT { 
          getEventlistT :: Context ctx -> m (a, HPrim u) }

type instance DUnit (Eventlist ctx u a) = u
type instance UCtx  (Eventlist ctx u)   = ctx

type instance DUnit (EventlistT ctx u m a) = u
type instance UCtx  (EventlistT ctx u m)   = ctx

type DEventlist ctx a    = Eventlist ctx Double a
type DEventlistT ctx m a = EventlistT ctx Double m a



-- Functor

instance Functor (Eventlist ctx u) where
  fmap f ma = Eventlist $ \ctx -> 
                let (a,w) = getEventlist ma ctx in (f a,w)


instance Monad m => Functor (EventlistT ctx u m) where
  fmap f ma = EventlistT $ \ctx -> 
                getEventlistT ma ctx >>= \(a,w) -> return (f a,w)



-- Applicative

instance Applicative (Eventlist ctx u) where
  pure a    = Eventlist $ \_   -> (a, mempty)
  mf <*> ma = Eventlist $ \ctx -> 
                let (f,w1) = getEventlist mf ctx
                    (a,w2) = getEventlist ma ctx
                in (f a, w1 `mappend` w2)


instance Monad m => Applicative (EventlistT ctx u m) where
  pure a    = EventlistT $ \_   -> return (a,mempty)
  mf <*> ma = EventlistT $ \ctx -> 
                getEventlistT mf ctx >>= \(f,w1) ->
                getEventlistT ma ctx >>= \(a,w2) ->
                return (f a, w1 `mappend` w2)

-- Monad

instance Monad (Eventlist ctx u) where
  return a  = Eventlist $ \_   -> (a, mempty)
  ma >>= k  = Eventlist $ \ctx -> 
                let (a,w1) = getEventlist ma ctx
                    (b,w2) = (getEventlist . k) a ctx
                in (b,w1 `mappend` w2)
                               



instance Monad m => Monad (EventlistT ctx u m) where
  return a  = EventlistT $ \_   -> return (a, mempty)
  ma >>= k  = EventlistT $ \ctx -> 
                getEventlistT ma ctx      >>= \(a,w1) ->
                (getEventlistT . k) a ctx >>= \(b,w2) -> 
                return (b, w1 `mappend` w2)
                                 



-- TraceM 
--
-- Note -  @ state `mappend` a @ means the first expression in a 
-- monadic drawing is the first element in the output file. It is
-- also \*\* at the back \*\* in the the Z-Order.
--
-- Some control over the Z-Order, possibly by adding /layers/ to 
-- the drawing model would be valuable. 
-- 

instance TraceM (Eventlist ctx u) where
  trace a = Eventlist $ \_ -> ((), a)

instance Monad m => TraceM (EventlistT ctx u m) where
  trace a = EventlistT $ \_ -> return ((), a)


-- ContextM

instance ContextM (Eventlist ctx u) where
  askCtx          = Eventlist $ \ctx -> (ctx, mempty)
  asksCtx f       = Eventlist $ \ctx -> (f ctx, mempty)
  localize upd ma = Eventlist $ \ctx -> getEventlist ma (upd ctx)



instance Monad m => ContextM (EventlistT ctx u m) where
  askCtx          = EventlistT $ \ctx -> return (ctx, mempty)
  asksCtx f       = EventlistT $ \ctx -> return (f ctx, mempty)
  localize upd ma = EventlistT $ \ctx -> getEventlistT ma (upd ctx)




runEventlist :: Context ctx -> Eventlist ctx u a -> (a, HPrim u)
runEventlist ctx ma = getEventlist ma ctx

-- | Run the notelist returning only the output it produces, drop
-- any answer from the monadic computation.
--
execEventlist :: Context ctx -> Eventlist ctx u a -> HPrim u
execEventlist ctx ma = snd $ runEventlist ctx ma

-- | Run the notelist ignoring the output it produces, return the 
-- answer from the monadic computation.
--
-- Note - this useful for testing, generally one would want the 
-- opposite behaviour (return the notelist, ignore than the 
-- answer).
-- 
evalEventlist :: Context ctx -> Eventlist ctx u a -> a
evalEventlist ctx ma = fst $ runEventlist ctx ma



runEventlistT :: Monad m 
             => Context ctx -> EventlistT ctx u m a -> m (a, HPrim u) 
runEventlistT ctx ma = getEventlistT ma ctx

execEventlistT :: Monad m 
              => Context ctx -> EventlistT ctx u m a -> m (HPrim u)
execEventlistT ctx ma = liftM snd $ runEventlistT ctx ma


evalEventlistT :: Monad m 
              => Context ctx -> EventlistT ctx u m a -> m a
evalEventlistT ctx ma = liftM fst $ runEventlistT ctx ma






--------------------------------------------------------------------------------





-- | Add an event to the note list taking settings from the 
-- /context/. 
--
-- This function is the /forgetful/ version of 'notei'. 
-- Commonly, it is used to add 'UEvent' objects which 
-- have no /answer/.
-- 
event :: ( TraceM m, ContextM m
         , ctx ~ UCtx m, u ~ DUnit (m ()) ) 
     => Event ctx u a -> m ()
event gf = askCtx >>= \ctx -> 
           let (PrimW o _) = runEvent ctx gf
           in trace (singleH o) >> return ()




-- | Add an event to the note list taking settings from the 
-- /context/. 
-- 
-- The event is logged in the Trace monad, and the result is 
-- returned.
-- 
eventi :: ( TraceM m, ContextM m
          , ctx ~ UCtx m, u ~ DUnit (m ()) ) 
       => Event ctx u a -> m a
eventi gf = askCtx >>= \ctx -> 
            let (PrimW o a) = runEvent ctx gf 
            in trace (singleH o) >> return a
            


-- | Add an event to the note list at the supplied onset, taking
-- settings from the /context/. 
--
-- This function is the /forgetful/ version of 'notei'. 
-- Commonly, it is used to add 'ULocEvent' objects which 
-- have no /answer/.
-- 
eventl :: ( TraceM m, InterpretUnit u, ContextM m
          , ctx ~ UCtx m, u ~ DUnit (m ()) ) 
      => u -> LocEvent ctx u a -> m ()
eventl ot evt = eventli ot evt >> return ()



-- | Add an event to the note list at the supplied onset, taking
-- settings from the /context/. 
--
-- The event is logged in the Trace monad, and the result is 
-- returned.
-- 
eventli :: ( TraceM m, InterpretUnit u, ContextM m
           , ctx ~ UCtx m, u ~ DUnit (m ()) ) 
        => u -> LocEvent ctx u a -> m a
eventli ot gf = askCtx >>= \ctx -> 
                let (PrimW o a) = runLocEvent ot ctx gf
                in trace (singleH o) >> return a


--
-- Design note
--
-- There doesn\'t seem to be much merit to providing an analogue 
-- to grid in Wumpus-Basic. An analogue could likely be achieved 
-- just by using a different unit for onset time.
--  

