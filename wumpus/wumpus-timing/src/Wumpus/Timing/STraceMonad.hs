{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Timing.STraceMonad
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- For Wumpus-basic?
--
--------------------------------------------------------------------------------

module Wumpus.Timing.STraceMonad
  (

    STrace
  , STraceT
  , STraceM(..)

  , runSTrace
  , runSTraceT
 
  ) where

import MonadLib ( MonadT(..) )          -- package: monadLib

import Wumpus.Basic.Utils.HList

import Control.Applicative



newtype STrace  i   a = STrace  { getSTrace  :: H i -> (a, H i) }

newtype STraceT i m a = STraceT { getSTraceT :: H i -> m (a, H i) }

-- Functor

instance Functor (STrace i) where
  fmap f m = STrace $ \w -> let (a,w') = getSTrace m w in (f a, w')

instance Monad m => Functor (STraceT i m) where
  fmap f m = STraceT $ \w -> getSTraceT m w >>= \(a,w') ->
                             return (f a, w')

-- Applicative

instance Applicative (STrace i) where
  pure a    = STrace $ \w -> (a,w)
  mf <*> ma = STrace $ \w -> let (f,w')  = getSTrace mf w 
                                 (a,w'') = getSTrace ma w'
                             in (f a,w'')


instance Monad m => Applicative (STraceT i m) where
  pure a    = STraceT $ \w -> return (a,w)
  mf <*> ma = STraceT $ \w -> getSTraceT mf w  >>= \(f,w')  ->
                              getSTraceT ma w' >>= \(a,w'') ->
                              return (f a,w'') 

-- Monad

instance Monad (STrace i) where
  return a = STrace $ \w -> (a,w)
  m >>= k  = STrace $ \w -> let (a,w') = getSTrace m w
                            in (getSTrace . k) a w'
     


instance Monad m => Monad (STraceT i m) where
  return a = STraceT $ \w -> return (a,w)
  m >>= k  = STraceT $ \w -> getSTraceT m w        >>= \(a,w')  ->
                             (getSTraceT . k) a w' >>= \(b,w'') ->
                             return (b,w'')




instance MonadT (STraceT i) where 
  lift m = STraceT $ \w -> m >>= \ a -> return (a,w)

class STraceM  m i | m -> i where
  strace  :: H i -> m ()
  strace1 :: i -> m ()


instance STraceM (STrace i) i where
  strace  h = STrace $ \w -> ((), w . h)  
  strace1 i = STrace $ \w -> ((), w `snocH` i)  

instance Monad m => STraceM (STraceT i m) i where
  strace  h = STraceT $ \w -> return ((), w . h)  
  strace1 i = STraceT $ \w -> return ((), w `snocH` i)  



runSTrace :: STrace i a -> (a,H i)
runSTrace mf = getSTrace mf id 

runSTraceT :: Monad m => STraceT i m a -> m (a,H i)
runSTraceT mf = getSTraceT mf id >>= \(a,w) -> return (a,w)


