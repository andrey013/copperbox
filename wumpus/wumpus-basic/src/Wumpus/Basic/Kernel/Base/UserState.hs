{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Base.UserState
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- User state class for Drawing monads.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Base.UserState
  (

    UState
  , UserStateM(..)

  ) where


import Control.Applicative

type family UState m :: *


class (Applicative m, Monad m) => UserStateM (m :: * -> *) where
  getState    :: st ~ UState m  => m st
  setState    :: st ~ UState m  => st -> m ()
  updateState :: st ~ UState m  => (st -> st) -> m ()
   