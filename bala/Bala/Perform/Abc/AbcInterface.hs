
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.AbcInterface
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Typeclasses that must be implemented to emit Abc from Score.
--
--------------------------------------------------------------------------------

module Bala.Perform.AbcInterface where

import Bala.Format.Output.OutputAbc

class AbcPitch pch where
  mkAbcPitchLetter   :: pch -> AbcPitchLetter
  
  mkAbcAccidental    :: pch -> Maybe AbcAccidental
  
  