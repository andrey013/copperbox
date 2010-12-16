{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Core
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  As per dependencies.
--
-- Common interface to @ZMidi.Core@.
--
-- This is a /shim/ module re-exporting types and functions from
-- the exposed ZMidi-Core modules. Just import this module to use 
-- ZMidi-Core. 
--
--------------------------------------------------------------------------------


module ZMidi.Core
  (
    module ZMidi.Core.Datatypes
  , module ZMidi.Core.Pretty
  , module ZMidi.Core.ReadFile
  , module ZMidi.Core.VersionNumber
  , module ZMidi.Core.WriteFile
    
  ) where

import ZMidi.Core.Datatypes
import ZMidi.Core.Pretty
import ZMidi.Core.ReadFile
import ZMidi.Core.VersionNumber
import ZMidi.Core.WriteFile

