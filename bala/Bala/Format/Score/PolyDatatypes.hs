{-# LANGUAGE ExistentialQuantification #-}


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
-- Datatypes for Score format with inline polyphony.
--
--------------------------------------------------------------------------------

{- OBSOLETE -}

module Bala.Format.Score.PolyDatatypes where


import Data.Sequence


data PScScore pch dur = PScScore (Seq (PScPart pch dur))

data PScPart pch dur = PScPart {
    psc_part_number         :: Int,
    psc_part_body           :: PScLine pch dur
  }

type PScLine pch dur = Seq (PScPolyUnit pch dur)


newtype PScPolyUnit pch dur  = PScPolyUnit { 
    getPolyLines :: [PScSegment pch dur] 
  }

-- Segments are inefficient for Abc which has a measure as a unit size,
-- whereas with LilyPond the unit size can be as long as the part.
-- For simplicity we use Segment for both representations.
-- 
-- (Actually polyphony can be as short as a note in LilyPond but the shortest
-- we consider rendering to is a measure).  
newtype PScSegment pch dur = PScSegment { 
    getPScSegment :: Seq (PScMeasure pch dur) 
  }


-- Measures just a unit in this representation - they don't contain pointers
-- to simultaneous measures, but the do track wich polyphonic voice they 
-- were originally associated with  
data PScMeasure pch dur = PScMeasure {
    psc_measure_number  :: Int,
    
    psc_measure_voice_number :: Int,
    -- the glyphs (notes, rests, ...) that make up the measure 
    psc_measure_glyphs  :: (Seq (PScGlyph pch dur))
  }

data PScGroupType = PScBeam | PScChord | PScGraceNotes
  deriving (Eq)
    
data PScGlyph pch dur = PScNote (PScPitch pch) dur
                      | PScRest dur
                      | PScSpacer dur  -- non-printed rest
                      | PScGroup PScGroupType [PScGlyph pch dur]
                 {-    | ScTaggedGlyph ScTag  -}
{-
-- tag things that aren't processed
newtype ScTag = ScTag Int
-}

data PScPitch pch = PScPitch pch


  