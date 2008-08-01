
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

module Bala.Perform.Abc.Class (PitchAbc(..), DurationAbc(..)) where

import Bala.Format.Output.OutputAbc

class PitchAbc pch where
  abcPitchLetter   :: pch -> AbcPitchLetter
  abcAccidental    :: pch -> Maybe AbcAccidental
  
class (Eq dur) => DurationAbc dur where
  asRational       :: dur -> Rational
