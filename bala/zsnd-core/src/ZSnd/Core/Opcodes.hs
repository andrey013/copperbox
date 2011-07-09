{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Core.Opcodes
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Csound opcodes.
-- 
-- Unlike Haskell, Csound allows optional arguments in opcode 
-- signatures. ZSnd models this by defining two versions of 
-- such opcodes:
-- 
-- a) The short version with no optional args uses the regular 
-- Csound name.
-- 
-- b) The fully specified version, which includes all optional
-- args, suffixes the Csound name with an underscore.
--
-- Note, there are many name clashes with Prelude. 
--
-- Also note, some opcodes are actually class methods to support 
-- rate overloading. However, whilst the class methods are 
-- exported, the class isn'\t. This is because the set of 
-- instances is finite - one or more of @IRate@, @KRate@ or
-- @ARate@.
-- 
--
--------------------------------------------------------------------------------

module ZSnd.Core.Opcodes
  (
   
    module ZSnd.Core.Opcodes.Base
  , module ZSnd.Core.Opcodes.SignalGenerators
  , module ZSnd.Core.Opcodes.SignalModifiers
  , module ZSnd.Core.Opcodes.Zak

  ) where

import ZSnd.Core.Opcodes.Base
import ZSnd.Core.Opcodes.SignalGenerators
import ZSnd.Core.Opcodes.SignalModifiers
import ZSnd.Core.Opcodes.Zak


