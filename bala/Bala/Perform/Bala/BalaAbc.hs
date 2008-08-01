--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.Bala.BalaAbc
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Use datatypes from Bala.Base libraries to implement the Abc interfaces.
--
--------------------------------------------------------------------------------


module Bala.Perform.Bala.BalaAbc where

import Bala.Base
import Bala.Format.Output.OutputAbc hiding (Sequence)
import Bala.Perform.Abc.Class

instance PitchAbc Pitch where
  abcPitchLetter   = abcPitchLetter'
  abcAccidental    = accidentalAttr . pitchAccidental
  
instance DurationAbc Duration where
  quaternoteAbc   = quarter
  asRational      = rationalize

  
abcPitchLetter' :: Pitch -> AbcPitchLetter
abcPitchLetter' = toEnum . fromEnum . pitchLetter



accidentalAttr :: Accidental -> Maybe AbcAccidental
accidentalAttr (DoubleFlat)     = Just doubleFlat 
accidentalAttr (Flat)           = Just flat  
accidentalAttr (Sharp)          = Just sharp 
accidentalAttr (DoubleSharp)    = Just doubleSharp 
accidentalAttr _                = Nothing
