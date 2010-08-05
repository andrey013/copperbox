{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Monads.TraceMonad
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- \"Trace\" monad and monad transformer.
--
-- Trace is operationally similar to the Writer monad but it
-- supports elementary consing as well as the Writer\'s monoidal 
-- concatenation. 
--
-- Note, some care is needed to order the output to a trace with
-- respect to the Z-order of a drawing. The API here may well
-- be too limited... 
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Monads.TraceMonad
  (

    Trace
  , TraceT

  , runTrace
  , runTraceT
 
  ) where

import Wumpus.Basic.Monads.DrawingCtxClass
import Wumpus.Basic.Monads.TraceClass
import Wumpus.Basic.Monads.TurtleClass
import Wumpus.Basic.Utils.HList


import MonadLib ( MonadT(..) )          -- package: monadLib

import Control.Applicative



newtype Trace  i   a = Trace  { getTrace  :: H i -> (a, H i) }

newtype TraceT i m a = TraceT { getTraceT :: H i -> m (a, H i) }

-- Functor

instance Functor (Trace i) where
  fmap f m = Trace $ \w -> let (a,w') = getTrace m w in (f a, w')

instance Monad m => Functor (TraceT i m) where
  fmap f m = TraceT $ \w -> getTraceT m w >>= \(a,w') ->
                            return (f a, w')

-- Applicative

instance Applicative (Trace i) where
  pure a    = Trace $ \w -> (a,w)
  mf <*> ma = Trace $ \w -> let (f,w')  = getTrace mf w 
                                (a,w'') = getTrace ma w'
                            in (f a,w'')


instance Monad m => Applicative (TraceT i m) where
  pure a    = TraceT $ \w -> return (a,w)
  mf <*> ma = TraceT $ \w -> getTraceT mf w  >>= \(f,w')  ->
                             getTraceT ma w' >>= \(a,w'') ->
                             return (f a,w'') 

-- Monad

instance Monad (Trace i) where
  return a = Trace $ \w -> (a,w)
  m >>= k  = Trace $ \w -> let (a,w') = getTrace m w
                           in (getTrace . k) a w'
     


instance Monad m => Monad (TraceT i m) where
  return a = TraceT $ \w -> return (a,w)
  m >>= k  = TraceT $ \w -> getTraceT m w        >>= \(a,w')  ->
                            (getTraceT . k) a w' >>= \(b,w'') ->
                            return (b,w'')




instance MonadT (TraceT i) where 
  lift m = TraceT $ \w -> m >>= \ a -> return (a,w)


instance TraceM (Trace i) i where
  trace  h = Trace $ \w -> ((), h . w)  
  trace1 i = Trace $ \w -> ((), i `consH` w)  

instance Monad m => TraceM (TraceT i m) i where
  trace  h = TraceT $ \w -> return ((), h . w)  
  trace1 i = TraceT $ \w -> return ((), i `consH` w)  



runTrace :: Trace i a -> (a,H i)
runTrace mf = getTrace mf id 

runTraceT :: Monad m => TraceT i m a -> m (a,H i)
runTraceT mf = getTraceT mf id >>= \(a,w) -> return (a,w)


--------------------------------------------------------------------------------
-- Cross instances

instance DrawingCtxM m => DrawingCtxM (TraceT i m) where
  askDrawingCtx   = TraceT $ \w -> askDrawingCtx >>= \ ctx -> return (ctx,w)
  localCtx ctx mf = TraceT $ \w -> localCtx ctx (getTraceT mf w)


instance TurtleM m => TurtleM (TraceT i m) where
  getLoc          = TraceT $ \w -> getLoc >>= \a -> return (a,w)
  setLoc c        = TraceT $ \w -> setLoc c >> return ((),w)
  getOrigin       = TraceT $ \w -> getOrigin >>= \a -> return (a,w)
  setOrigin o     = TraceT $ \w -> setOrigin o >> return ((),w)

instance TurtleScaleM m u => TurtleScaleM (TraceT i m) u where
  xStep           = TraceT $ \w -> xStep >>= \a -> return (a,w)
  yStep           = TraceT $ \w -> yStep >>= \a -> return (a,w)
 

