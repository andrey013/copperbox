
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.Score.Datatypes
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Datatypes for Score format
--
--------------------------------------------------------------------------------

module Bala.Format.Score.Datatypes  where

import Bala.Base

import Data.Sequence

-- tag things that aren't processed
newtype ScTag = ScTag Integer

data ScScore = ScScore (Seq ScPart)

data ScPart = ScPart Integer (Seq ScBar) -- for the time being

data ScBar = ScBar Integer (Seq ScGlyph)

data ScGroupType = ScBeam | ScChord | ScGraceNotes
  deriving (Eq)

data ScGlyph = ScNote PitchName Int Double
             | ScRest Double
             | ScGroup ScGroupType [ScGlyph]
             | ScRef ScTag
           
           

   