{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  NotateMonad
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  mptc, flexible instances.
--
-- Notate Monad - a state transformer over a reader monad.
--
--------------------------------------------------------------------------------


module NotateMonad (
    NotateM(..),
    runNotate,
    evalNotate
  ) where


import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State






-- | The Notate monad is a state transformer over a reader monad.
newtype NotateM st env a = NotateM { getNotate :: StateT st (Reader env) a }


instance Functor (NotateM st env) where
  fmap f m = NotateM $ fmap f (getNotate m)

instance Monad (NotateM st env) where
  return a = NotateM $ return a
  (>>=) m k = NotateM $ getNotate m >>= getNotate . k


instance MonadState st (NotateM st env) where
  get   = NotateM get
  put s = NotateM (put s)

instance MonadReader env (NotateM st env) where
  ask         = NotateM $ lift ask
  local f m   = NotateM $ local f (getNotate m)

instance Applicative (NotateM st env) where
  pure = return
  (<*>) = ap


runNotate :: NotateM st env a -> st -> env -> (a, st)
runNotate (NotateM f) st env = runReader (runStateT f st) env



evalNotate :: NotateM st env a -> st -> env -> a
evalNotate m st env = fst (runNotate m st env)
