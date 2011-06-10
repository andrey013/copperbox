{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Basic.Kernel.Objects.TraceNotelist
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Generate a note list with a /trace/ - a Writer monad collecting 
-- intermediate events - and /context/ - a reader monad of 
-- attributes - amplitude or whatever.
--
--------------------------------------------------------------------------------

module ZSnd.Basic.Kernel.Objects.TraceNotelist
  (

  -- * Collect primitives (writer monad) 
    TraceM(..)

  , Score
  , execScore

  , Notelist
  , DNotelist
  , NotelistT
  , DNotelistT

  , runNotelist
  , execNotelist
  , evalNotelist

  , runNotelistT
  , execNotelistT
  , evalNotelistT

  , event
  , eventi
  , eventl
  , eventli


  ) where


import ZSnd.Basic.Kernel.Base.BaseDefs
import ZSnd.Basic.Kernel.Base.Context
import ZSnd.Basic.Kernel.Base.WrappedPrimitive
import ZSnd.Basic.Kernel.Objects.Basis
import ZSnd.Basic.Kernel.Objects.LocEvent


import ZSnd.Core                                -- package: zsnd-core

import Control.Applicative
import Control.Monad
import Data.Monoid


--------------------------------------------------------------------------------



newtype Score = Score { getScore :: ScoBuilder () }


instance Monoid Score where
  mempty        = Score $ return ()
  a `mappend` b = Score $ getScore a >> getScore b


execScore :: Score -> Section
execScore = runScoBuilder . getScore


-- | Collect elementary events as part of a larger score.
--
-- TraceM works much like a writer monad.
--
class TraceM (m :: * -> *) where
  trace     :: DUnit (m ()) ~ u => HPrim u -> m ()




newtype Notelist ctx u a   = Notelist { 
          getNotelist :: ctx -> (a, HPrim u) }

newtype NotelistT ctx u m a = NotelistT { 
          getNotelistT :: ctx -> m (a, HPrim u) }

type instance DUnit (Notelist ctx u a) = u
type instance DUnit (NotelistT ctx u m a) = u

type instance Ctx (Notelist ctx u) = ctx
type instance Ctx (NotelistT ctx u m) = ctx


type DNotelist a    = Notelist Double a
type DNotelistT m a = NotelistT Double m a



-- Functor

instance Functor (Notelist ctx u) where
  fmap f ma = Notelist $ \ctx -> 
                let (a,w) = getNotelist ma ctx in (f a,w)


instance Monad m => Functor (NotelistT ctx u m) where
  fmap f ma = NotelistT $ \ctx -> 
                getNotelistT ma ctx >>= \(a,w) -> return (f a,w)



-- Applicative

instance Applicative (Notelist ctx u) where
  pure a    = Notelist $ \_   -> (a, mempty)
  mf <*> ma = Notelist $ \ctx -> 
                let (f,w1) = getNotelist mf ctx
                    (a,w2) = getNotelist ma ctx
                in (f a, w1 `mappend` w2)


instance Monad m => Applicative (NotelistT ctx u m) where
  pure a    = NotelistT $ \_   -> return (a,mempty)
  mf <*> ma = NotelistT $ \ctx -> 
                getNotelistT mf ctx >>= \(f,w1) ->
                getNotelistT ma ctx >>= \(a,w2) ->
                return (f a, w1 `mappend` w2)

-- Monad

instance Monad (Notelist ctx u) where
  return a  = Notelist $ \_   -> (a, mempty)
  ma >>= k  = Notelist $ \ctx -> 
                let (a,w1) = getNotelist ma ctx
                    (b,w2) = (getNotelist . k) a ctx
                in (b,w1 `mappend` w2)
                               



instance Monad m => Monad (NotelistT ctx u m) where
  return a  = NotelistT $ \_   -> return (a, mempty)
  ma >>= k  = NotelistT $ \ctx -> 
                getNotelistT ma ctx      >>= \(a,w1) ->
                (getNotelistT . k) a ctx >>= \(b,w2) -> 
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

instance TraceM (Notelist ctx u) where
  trace a = Notelist $ \_ -> ((), a)

instance Monad m => TraceM (NotelistT ctx u m) where
  trace a = NotelistT $ \_ -> return ((), a)


-- ContextM

instance ContextM (Notelist ctx u) where
  askCtx          = Notelist $ \ctx -> (ctx, mempty)
  asksCtx f       = Notelist $ \ctx -> (f ctx, mempty)
  localize upd ma = Notelist $ \ctx -> getNotelist ma (upd ctx)



instance Monad m => ContextM (NotelistT ctx u m) where
  askCtx          = NotelistT $ \ctx -> return (ctx, mempty)
  asksCtx f       = NotelistT $ \ctx -> return (f ctx, mempty)
  localize upd ma = NotelistT $ \ctx -> getNotelistT ma (upd ctx)




runNotelist :: ctx -> Notelist ctx u a -> (a, Score)
runNotelist ctx ma = post $ getNotelist ma ctx
  where
    post (a,hf) = (a, liftToScore hf)

-- | Run the notelist returning only the output it produces, drop
-- any answer from the monadic computation.
--
execNotelist :: ctx -> Notelist ctx u a -> Score
execNotelist ctx ma = snd $ runNotelist ctx ma

-- | Run the notelist ignoring the output it produces, return the 
-- answer from the monadic computation.
--
-- Note - this useful for testing, generally one would want the 
-- opposite behaviour (return the notelist, ignore than the 
-- answer).
-- 
evalNotelist :: ctx -> Notelist ctx u a -> a
evalNotelist ctx ma = fst $ runNotelist ctx ma



runNotelistT :: Monad m 
             => ctx -> NotelistT ctx u m a -> m (a, Score) 
runNotelistT ctx ma = liftM post $ getNotelistT ma ctx
  where
    post (a,hf) = (a, liftToScore hf)

execNotelistT :: Monad m 
              => ctx -> NotelistT ctx u m a -> m Score
execNotelistT ctx ma = liftM snd $ runNotelistT ctx ma


evalNotelistT :: Monad m 
              => ctx -> NotelistT ctx u m a -> m a
evalNotelistT ctx ma = liftM fst $ runNotelistT ctx ma




-- Note unlike Wumpus, empty note lists pose no problem for ZSnd.



-- | /Unsafe/ promotion of @HPrim@ to @Picture@.
--
-- If the HPrim is empty, a run-time error is thrown.
-- 
liftToScore :: HPrim u -> Score
liftToScore = Score . outputNotes . hprimToList




--------------------------------------------------------------------------------





-- | Add an event to the note list taking settings from the 
-- /context/. 
--
-- This function is the /forgetful/ version of 'notei'. 
-- Commonly, it is used to add 'UEvent' objects which 
-- have no /answer/.
-- 
event :: (TraceM m, ContextM m, u ~ DUnit (m ()), ctx ~ Ctx m ) 
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
eventi :: (TraceM m, ContextM m, u ~ DUnit (m ()), ctx ~ Ctx m ) 
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
eventl :: ( TraceM m, InterpretUnit u, ContextM m, CtxTempo ctx
         , u ~ DUnit (m ()), ctx ~ Ctx m ) 
      => u -> LocEvent ctx u a -> m ()
eventl ot evt = eventli ot evt >> return ()



-- | Add an event to the note list at the supplied onset, taking
-- settings from the /context/. 
--
-- The event is logged in the Trace monad, and the result is 
-- returned.
-- 
eventli :: ( TraceM m, InterpretUnit u, ContextM m, CtxTempo ctx
          , u ~ DUnit (m ()), ctx ~ Ctx m ) 
        => u -> LocEvent ctx u a -> m a
eventli ot gf = askCtx >>= \ctx -> 
                let (PrimW o a) = runLocEvent ot ctx gf
                in trace (singleH o) >> return a


--
-- Design note
--
-- Currently there is no analogue to @Connector@ in Wumpus-Basic, 
-- but an interpolate object might have real value.
--
-- Conversely there doesn\'t seem to be much merit to providing an 
-- analogue to grid in Wumpus-Basic. An analogue could likely be
-- achieved just by using a different unit for onset time.
--  

