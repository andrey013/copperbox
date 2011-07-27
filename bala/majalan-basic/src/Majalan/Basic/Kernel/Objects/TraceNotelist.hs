{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Basic.Kernel.Objects.TraceNotelist
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
-- The note list a timeline - events can be freely placed on the 
-- timeline, they do not have to be sequential (the timeline is 
-- sorted before rendering).
-- 
--------------------------------------------------------------------------------

module Majalan.Basic.Kernel.Objects.TraceNotelist
  (

  -- * Collect primitives (output class) 
    TraceM(..)

  -- * Note list
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


import Majalan.Basic.Kernel.Base.BaseDefs
import Majalan.Basic.Kernel.Base.Context
import Majalan.Basic.Kernel.Base.WrappedPrimitive
import Majalan.Basic.Kernel.Objects.Basis
import Majalan.Basic.Kernel.Objects.LocEvent



import Control.Applicative
import Control.Monad
import Data.Monoid


--------------------------------------------------------------------------------




-- | Collect elementary events as part of a larger score.
--
-- TraceM works much like a writer monad.
--
class TraceM (m :: * -> *) where
  trace     :: (DUnit (m ()) ~ u, ITbl (m ()) ~ itbl) => HPrim itbl u -> m ()




newtype Notelist itbl ctx u a   = Notelist { 
          getNotelist :: Context ctx -> (a, HPrim itbl u) }

newtype NotelistT itbl ctx u m a = NotelistT { 
          getNotelistT :: Context ctx -> m (a, HPrim itbl u) }

type instance DUnit (Notelist itbl ctx u a) = u
type instance UCtx  (Notelist itbl ctx u)   = ctx
type instance ITbl  (Notelist itbl ctx u a) = itbl

type instance DUnit (NotelistT itbl ctx u m a) = u
type instance UCtx  (NotelistT itbl ctx u m)   = ctx
type instance ITbl  (NotelistT itbl ctx u m a) = itbl

type DNotelist itbl ctx a    = Notelist itbl ctx Double a
type DNotelistT itbl ctx m a = NotelistT itbl ctx Double m a



-- Functor

instance Functor (Notelist itbl ctx u) where
  fmap f ma = Notelist $ \ctx -> 
                let (a,w) = getNotelist ma ctx in (f a,w)


instance Monad m => Functor (NotelistT itbl ctx u m) where
  fmap f ma = NotelistT $ \ctx -> 
                getNotelistT ma ctx >>= \(a,w) -> return (f a,w)



-- Applicative

instance Applicative (Notelist itbl ctx u) where
  pure a    = Notelist $ \_   -> (a, mempty)
  mf <*> ma = Notelist $ \ctx -> 
                let (f,w1) = getNotelist mf ctx
                    (a,w2) = getNotelist ma ctx
                in (f a, w1 `mappend` w2)


instance Monad m => Applicative (NotelistT itbl ctx u m) where
  pure a    = NotelistT $ \_   -> return (a,mempty)
  mf <*> ma = NotelistT $ \ctx -> 
                getNotelistT mf ctx >>= \(f,w1) ->
                getNotelistT ma ctx >>= \(a,w2) ->
                return (f a, w1 `mappend` w2)

-- Monad

instance Monad (Notelist itbl ctx u) where
  return a  = Notelist $ \_   -> (a, mempty)
  ma >>= k  = Notelist $ \ctx -> 
                let (a,w1) = getNotelist ma ctx
                    (b,w2) = (getNotelist . k) a ctx
                in (b,w1 `mappend` w2)
                               



instance Monad m => Monad (NotelistT itbl ctx u m) where
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

instance TraceM (Notelist itbl ctx u) where
  trace a = Notelist $ \_ -> ((), a)

instance Monad m => TraceM (NotelistT itbl ctx u m) where
  trace a = NotelistT $ \_ -> return ((), a)


-- ContextM

instance ContextM (Notelist itbl ctx u) where
  askCtx          = Notelist $ \ctx -> (ctx, mempty)
  asksCtx f       = Notelist $ \ctx -> (f ctx, mempty)
  localize upd ma = Notelist $ \ctx -> getNotelist ma (upd ctx)



instance Monad m => ContextM (NotelistT itbl ctx u m) where
  askCtx          = NotelistT $ \ctx -> return (ctx, mempty)
  asksCtx f       = NotelistT $ \ctx -> return (f ctx, mempty)
  localize upd ma = NotelistT $ \ctx -> getNotelistT ma (upd ctx)




runNotelist :: Context ctx -> Notelist itbl ctx u a -> (a, HPrim itbl u)
runNotelist ctx ma = getNotelist ma ctx

-- | Run the notelist returning only the output it produces, drop
-- any answer from the monadic computation.
--
execNotelist :: Context ctx -> Notelist itbl ctx u a -> HPrim itbl u
execNotelist ctx ma = snd $ runNotelist ctx ma

-- | Run the notelist ignoring the output it produces, return the 
-- answer from the monadic computation.
--
-- Note - this useful for testing, generally one would want the 
-- opposite behaviour (return the notelist, ignore than the 
-- answer).
-- 
evalNotelist :: Context ctx -> Notelist itbl ctx u a -> a
evalNotelist ctx ma = fst $ runNotelist ctx ma



runNotelistT :: Monad m 
             => Context ctx -> NotelistT itbl ctx u m a -> m (a, HPrim itbl u) 
runNotelistT ctx ma = getNotelistT ma ctx

execNotelistT :: Monad m 
              => Context ctx -> NotelistT itbl ctx u m a -> m (HPrim itbl u)
execNotelistT ctx ma = liftM snd $ runNotelistT ctx ma


evalNotelistT :: Monad m 
              => Context ctx -> NotelistT itbl ctx u m a -> m a
evalNotelistT ctx ma = liftM fst $ runNotelistT ctx ma






--------------------------------------------------------------------------------





-- | Add an event to the note list taking settings from the 
-- /context/. 
--
-- This function is the /forgetful/ version of 'notei'. 
-- Commonly, it is used to add 'UEvent' objects which 
-- have no /answer/.
-- 
event :: ( TraceM m, ContextM m
         , ctx ~ UCtx m, u ~ DUnit (m ()), itbl ~ ITbl (m ())) 
     => Event itbl ctx u a -> m ()
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
          , ctx ~ UCtx m, u ~ DUnit (m ()), itbl ~ ITbl (m ())) 
       => Event itbl ctx u a -> m a
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
          , ctx ~ UCtx m, u ~ DUnit (m ()), itbl ~ ITbl (m ())) 
      => u -> LocEvent itbl ctx u a -> m ()
eventl ot evt = eventli ot evt >> return ()



-- | Add an event to the note list at the supplied onset, taking
-- settings from the /context/. 
--
-- The event is logged in the Trace monad, and the result is 
-- returned.
-- 
eventli :: ( TraceM m, InterpretUnit u, ContextM m
           , ctx ~ UCtx m, u ~ DUnit (m ()), itbl ~ ITbl (m ()) ) 
        => u -> LocEvent itbl ctx u a -> m a
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

