{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE KindSignatures             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Basic.Kernel.Objects.TraceOutput
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output via a trace (i.e a writer monad).
--
--------------------------------------------------------------------------------

module ZMidi.Basic.Kernel.Objects.TraceOutput
  ( 
    TraceM(..)
  
  ) where

import ZMidi.Basic.Kernel.Base.BaseDefs
import ZMidi.Basic.Kernel.Base.WrappedPrimitive

-- | Collect elementary events as part of a larger composition.
--
-- TraceM works much like a writer monad.
--
class TraceM (m :: * -> *) where
  trace     :: Durational (m ()) ~ u => HPrim u -> m ()