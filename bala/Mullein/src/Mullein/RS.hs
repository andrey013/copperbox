{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.RS
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Reader State monad
--
--------------------------------------------------------------------------------

module Mullein.RS where

import Mullein.Utils

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State


newtype RS st env a = RS { getRS :: ReaderT env (State st) a }
  deriving (Functor, Monad, MonadReader env, MonadState st)

runRS :: RS st env a -> st -> env -> (a,st)
runRS f st env = runState (runReaderT (getRS f) env) st

evalRS :: RS st env a -> st -> env -> a
evalRS = fst `ooo` runRS

instance Applicative (RS st env) where
  pure = return
  (<*>) = ap