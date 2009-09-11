{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.Extended
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Extended music representation datatypes (e.g. pitch with fingering)
--
--------------------------------------------------------------------------------

module Mullein.Extended 
  (
  -- * Notes with fingering annotations
    Finger
  , FingeredPitch(..)
  , FingeredGlyph
  , finger
  
  -- * LilyPond drum pitches
  , DrumPitch(..)
  , DrumGlyph

  -- * LilyPond /spacer marks/
  , SpacerMark(..)
  , SpacerGlyph
  , SpacerAnnotation
  , Direction(..)
  , markupAboveSpacer
  , markupBelowSpacer
  , markupCenterSpacer

  -- * Extended Glyph, pitch annotated with  string number
  , TabGlyph
  , PitchPlusString(..)
  , StringNumber

  ) where

import Mullein.Bracket ( ExtBeam(..) )
import Mullein.Core
import Mullein.Duration
import Mullein.LilyPondDoc
import Mullein.LilyPondOutput
import Mullein.Pitch
import Mullein.Utils ( optDoc )

import Text.PrettyPrint.Leijen

--------------------------------------------------------------------------------
-- Notes with fingering annotations

type Finger = Int

-- Fingering is optional - numering every note would clutter up
-- scores, especially where notes are repeated.

data FingeredPitch = FingeredPitch { 
       finPitch    :: Pitch, 
       finNumber   :: Maybe Finger
     }
  deriving (Eq,Show)

type FingeredGlyph = Glyph FingeredPitch Duration


finger :: FingeredGlyph -> Int -> FingeredGlyph
finger (Note (FingeredPitch p _) drn t)  i = Note (FingeredPitch p (Just i)) drn t
finger a                                 _ = a



instance HasPitch FingeredPitch where
  getPitch (FingeredPitch p _)= p
  setPitch p (FingeredPitch _ oi) = FingeredPitch p oi


instance MakeNote FingeredGlyph where
  makeNote pch drn = Note (FingeredPitch pch Nothing) drn False

instance MakeRest FingeredGlyph where
  makeRest drn = Rest drn


instance LilyPondGlyph (Glyph FingeredPitch (Maybe Duration)) where
  lyGlyph (Note p d t)     = pitchDurationFinger p d <> optDoc t tie
  lyGlyph (Rest d)         = rest d
  lyGlyph (Spacer d)       = spacer d
  lyGlyph (Chord ps d t)   = chordForm (map pitchFinger ps) d <> optDoc t tie
  lyGlyph (GraceNotes xs)  = graceForm $ map fn xs 
    where fn (GraceNote p d) = pitchDurationFinger p d


pitchDurationFinger :: FingeredPitch -> Maybe Duration -> Doc
pitchDurationFinger (FingeredPitch p mi) md = 
    pitch p <> maybe empty duration md <> maybe empty fingering mi

pitchFinger :: FingeredPitch -> Doc
pitchFinger (FingeredPitch p mi) = pitch p <> maybe empty fingering mi

fingering :: Finger -> Doc
fingering i = char '-' <> int i



--------------------------------------------------------------------------------
-- LilyPond drum pitches 

data DrumPitch = DrumPitch { 
      drumLongName   :: String, 
      drumShortName  :: String 
    }
  deriving (Eq,Show)


type DrumGlyph = Glyph DrumPitch Duration


instance MakeRest DrumGlyph where
  makeRest drn = Rest drn


instance LilyPondGlyph (Glyph DrumPitch (Maybe Duration)) where
  lyGlyph = oLyGlyph (text . drumShortName)


--------------------------------------------------------------------------------
-- Spacer marks 

-- Spacer marks are an alternative to glyphs for LilyPond - they 
-- are useful to separate markup from glyphs. For example you can 
-- have a staff with two voice contexts: one context has the usual 
-- glyphs for the melody, and the second context has guitar 
-- chords (fretboard diagrams). To align the guitar chords with
-- the chord changes in the melody, the chords are /carried/ by
-- unprinted spacer rests.

data SpacerMark drn = SpacerMark (Maybe SpacerAnnotation) drn
  deriving (Show)

type SpacerGlyph = SpacerMark Duration

type SpacerAnnotation = (Direction,Doc)

data Direction = Above | Below | Center
  deriving (Eq,Show)

instance MakeRest SpacerGlyph where
  makeRest = SpacerMark Nothing


markupAboveSpacer :: Doc -> Duration -> SpacerGlyph
markupAboveSpacer doc = SpacerMark (Just (Above,doc))


markupBelowSpacer :: Doc -> Duration -> SpacerGlyph
markupBelowSpacer doc = SpacerMark (Just (Below,doc))

markupCenterSpacer :: Doc -> Duration -> SpacerGlyph
markupCenterSpacer doc = SpacerMark (Just (Center,doc))

instance LilyPondGlyph (SpacerMark (Maybe Duration)) where
  lyGlyph (SpacerMark Nothing md)        = spacer md
  lyGlyph (SpacerMark (Just (a,doc)) md) = fn a (spacer md) doc
    where
       fn Above  = (**^)
       fn Below  = (**\)
       fn Center = (**-) 
      
-- TODO in Bracket add a Bar transformation that doesn't beam
instance ExtBeam (SpacerMark dur) where
  outerElement (SpacerMark _ _)   = False


instance HasDuration SpacerMark where
  getDuration (SpacerMark _ d) = d


instance ChangeDurationLR SpacerMark where
  changeDurationLR d0 (SpacerMark a d) = (SpacerMark a (alterDuration d0 d), d)

--------------------------------------------------------------------------------
-- Tab Glyphs 

-- Notes are annotated with string number


type StringNumber = Int

type TabGlyph = Glyph PitchPlusString Duration

data PitchPlusString = PPS Pitch StringNumber
  deriving (Eq)

instance Show PitchPlusString where
  showsPrec prec (PPS p i) = showsPrec prec (p,i)


instance LilyPondGlyph (Glyph PitchPlusString (Maybe Duration)) where
  lyGlyph (Note p d t)     = pitchDurationString p d <> optDoc t tie
  lyGlyph (Rest d)         = rest d
  lyGlyph (Spacer d)       = spacer d
  lyGlyph (Chord ps d t)   = chordForm (map pitchPlusString ps) d 
                               <> optDoc t tie
  lyGlyph (GraceNotes xs)  = graceForm $ map fn xs 
    where fn (GraceNote p d) = pitchDurationString p d


pitchDurationString :: PitchPlusString -> Maybe Duration -> Doc
pitchDurationString (PPS p i) md = 
    pitch p <> maybe empty duration md <> stringnumber i

pitchPlusString :: PitchPlusString -> Doc
pitchPlusString (PPS p i) = pitch p <> stringnumber i

stringnumber :: StringNumber -> Doc
stringnumber i = char '\\' <> int i

instance MakeNote (StringNumber -> TabGlyph) where
  makeNote p d = \n -> Note (PPS p n) d False

instance MakeChord ([StringNumber] -> TabGlyph) where
  makeChord ps d = \ns -> Chord (zipWith PPS ps ns) d False

instance HasPitch PitchPlusString where
  getPitch (PPS p _)   = p
  setPitch p (PPS _ i) = PPS p i