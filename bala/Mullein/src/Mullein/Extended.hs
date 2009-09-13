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
  , FingeredGlyph
  , FingeredGlyph'
  , lyFingeredGlyph
  , finger
  
  -- * LilyPond drum pitches
  , DrumPitch(..)
  , DrumGlyph
  , DrumGlyph'
  , lyDrumGlyph


  -- * LilyPond /spacer marks/
  , SpacerMark(..)
  , SpacerGlyph
  , SpacerGlyph'
  , SpacerAnnotation
  , Direction(..)
  , lySpacerGlyph
  , markupAboveSpacer
  , markupBelowSpacer
  , markupCenterSpacer

  -- * Extended Glyph, pitch annotated with  string number
  , TabGlyph
  , TabGlyph'
  , lyTabGlyph
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

type FingeredGlyph  = Glyph (Maybe Finger) Pitch Duration
type FingeredGlyph' = Glyph (Maybe Finger) Pitch (Maybe Duration)  

finger :: FingeredGlyph -> Int -> FingeredGlyph
finger (Note _ p drn t)  i = Note (Just i) p drn t
finger a                 _ = a


instance MakeNote FingeredGlyph where
  makeNote pch drn = Note Nothing pch drn False


lyFingeredGlyph :: FingeredGlyph' -> Doc
lyFingeredGlyph (Note mi p d t)  = pitchFingerDuration p mi d <> optDoc t tie
lyFingeredGlyph (Rest d)         = rest d
lyFingeredGlyph (Spacer d)       = spacer d
lyFingeredGlyph (Chord ps d t)   = chordForm (map (uncurry pitchFinger) ps) d 
                                     <> optDoc t tie
lyFingeredGlyph (GraceNotes xs)  = graceForm $ map fn xs 
    where fn (GraceNote mi p d) = pitchFingerDuration p mi d


pitchFingerDuration :: Pitch -> Maybe Finger -> Maybe Duration -> Doc
pitchFingerDuration p mi md = 
    pitch p <> maybe empty duration md <> maybe empty fingering mi

pitchFinger :: Maybe Finger -> Pitch -> Doc
pitchFinger mi p = pitch p <> maybe empty fingering mi

fingering :: Finger -> Doc
fingering i = char '-' <> int i



--------------------------------------------------------------------------------
-- LilyPond drum pitches 

data DrumPitch = DrumPitch { 
      drumLongName   :: String, 
      drumShortName  :: String 
    }
  deriving (Eq,Show)


type DrumGlyph  = Glyph () DrumPitch Duration
type DrumGlyph' = Glyph () DrumPitch (Maybe Duration) 


lyDrumGlyph :: DrumGlyph' -> Doc
lyDrumGlyph = oLyGlyph (text . drumShortName)


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

type SpacerGlyph  = SpacerMark Duration
type SpacerGlyph' = SpacerMark (Maybe Duration)


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

lySpacerGlyph :: SpacerGlyph' -> Doc
lySpacerGlyph (SpacerMark Nothing md)        = spacer md
lySpacerGlyph (SpacerMark (Just (a,doc)) md) = fn a (spacer md) doc
    where
       fn Above  = (**^)
       fn Below  = (**\)
       fn Center = (**-) 

-- TODO in Bracket add a Bar transformation that doesn't beam
instance ExtBeam (SpacerMark dur) where
  outerElement (SpacerMark _ _)   = False


instance HasDuration SpacerMark where
  getDuration (SpacerMark _ d) = d


instance ChangeDurationLyRel SpacerMark where
  changeDurationLyRel d0 (SpacerMark a d) = (SpacerMark a (alterDuration d0 d), d)

--------------------------------------------------------------------------------
-- Tab Glyphs 

-- Notes are annotated with string number

-- TO CONSIDER...
-- Having a new type for each /output style/ puts a high burden 
-- on constructing and translating from one type to another. It 
-- might be preferable to get rid of the LilyPondGlyph class 
-- and parameterize the render functions with glyph-printer 
-- function.

type StringNumber = Int

type TabGlyph  = Glyph StringNumber Pitch Duration
type TabGlyph' = Glyph StringNumber Pitch (Maybe Duration)


lyTabGlyph :: TabGlyph' -> Doc
lyTabGlyph (Note i p d t)  = pitchDurationString p i d <> optDoc t tie
lyTabGlyph (Rest d)        = rest d
lyTabGlyph (Spacer d)      = spacer d
lyTabGlyph (Chord ps d t)  = chordForm (map (\(i,p) -> pitchPlusString p i) ps) d 
                               <> optDoc t tie
lyTabGlyph (GraceNotes xs) = graceForm $ map fn xs 
  where fn (GraceNote i p d) = pitchDurationString p i d


pitchDurationString :: Pitch -> StringNumber -> Maybe Duration -> Doc
pitchDurationString p i md = 
    pitch p <> maybe empty duration md <> stringnumber i

pitchPlusString :: Pitch -> StringNumber -> Doc
pitchPlusString p i = pitch p <> stringnumber i

stringnumber :: StringNumber -> Doc
stringnumber i = char '\\' <> int i



-- NOTE - these instances are due for removal.

instance MakeNote (StringNumber -> TabGlyph) where
  makeNote p d = \n -> Note n p d False

instance MakeChord ([StringNumber] -> TabGlyph) where
  makeChord ps d = \ns -> Chord (zip ns ps) d False

