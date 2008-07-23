
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.AScore.Datatypes
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Datatypes for A(bstract) Score format
--
--------------------------------------------------------------------------------

module Bala.Format.Score.Datatypes  where

import Bala.Base

newtype ScTag = ScTag Integer

data ScScore = ScScore [ScGlyph] -- for the time being

data ScBar = ScBar ScTag [ScGlyph]

data ScGroupType = ScBeam | ScChord | ScGraceNotes
  deriving (Eq)

data ScGlyph = ScNote ScTag PitchName Int Double
             | ScRest ScTag Double
             | ScGroup ScTag ScGroupType [ScGlyph]
           
           

   