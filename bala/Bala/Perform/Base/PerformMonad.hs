{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.Base.PerformMonad
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Perfrom Monad - a state transformer over a reader monad. 
--
--------------------------------------------------------------------------------


module Bala.Perform.Base.PerformMonad (
  PerformM(..),
  runPerform,
  evalPerform
  ) where

import Bala.Base
import Bala.Format.Output.OutputLilyPond

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State



    

  
-- | The Perform monad is a state transformer over a reader monad.   
newtype PerformM st env a = PerformM { getPerform :: StateT st (Reader env) a }


instance Functor (PerformM st env) where
  fmap f m = PerformM $ fmap f (getPerform m)
  
instance Monad (PerformM st env) where
  return a = PerformM $ return a
  (>>=) m k = PerformM $ getPerform m >>= getPerform . k


instance MonadState st (PerformM st env) where
  get   = PerformM get
  put s = PerformM (put s)
  
instance MonadReader env (PerformM st env) where
  ask   = PerformM $ lift ask
  local f m = PerformM $ local f (getPerform m) 

instance Applicative (PerformM st env) where
  pure = return
  (<*>) = ap
  
  
runPerform :: PerformM st env a -> st -> env -> (a, st)  
runPerform (PerformM f) st env = runReader (runStateT f st) env



evalPerform :: PerformM st env a -> st -> env -> a
evalPerform m st env = fst (runPerform m st env)
                       