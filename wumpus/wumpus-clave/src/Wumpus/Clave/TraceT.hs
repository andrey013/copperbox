{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Clave.TraceT
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Tracing monad transformer - operationally similar to a writer
-- monad, but with snoc-ing rather than monoidal append.
--
-- Candidate for Wumpus-Extra.
-- 
--------------------------------------------------------------------------------

module Wumpus.Clave.TraceT
  (

    TraceT
  , TraceM(..)
  , runTraceT
 
  ) where

import MonadLib ( MonadT(..) )          -- package: monadLib

import Wumpus.Clave.Utils

import Control.Applicative





newtype TraceT i m a = TraceT { getTraceT :: H i -> m (a, H i) }

instance Monad m => Functor (TraceT i m) where
  fmap f m = TraceT $ \w -> getTraceT m w >>= \(a,w') ->
                            return (f a, w')

instance Monad m => Applicative (TraceT i m) where
  pure a    = TraceT $ \w -> return (a,w)
  mf <*> ma = TraceT $ \w -> getTraceT mf w  >>= \(f,w')  ->
                             getTraceT ma w' >>= \(a,w'') ->
                             return (f a,w'') 


instance Monad m => Monad (TraceT i m) where
  return a = TraceT $ \w -> return (a,w)
  m >>= k  = TraceT $ \w -> getTraceT m w        >>= \(a,w')  ->
                            (getTraceT . k) a w' >>= \(b,w'') ->
                            return (b,w'')

instance MonadT (TraceT i) where 
  lift m = TraceT $ \w -> m >>= \ a -> return (a,w)

class TraceM  m i | m -> i where
  trace  :: H i -> m ()
  trace1 :: i -> m ()

instance Monad m => TraceM (TraceT i m) i where
  trace  h = TraceT $ \w -> return ((), h . w)  
  trace1 i = TraceT $ \w -> return ((), i `consH` w)  



runTraceT :: Monad m => TraceT i m a -> m (a,H i)
runTraceT mf = getTraceT mf id >>= \(a,w) -> return (a,w)

{-
trace ::  Monad m => H i -> TraceT i m ()
trace h = TraceT $ \w -> return ((), w . h)  


trace1 :: Monad m => i -> TraceT i m ()
trace1 i = TraceT $ \w -> return ((), w `snocH` i)  
-}

