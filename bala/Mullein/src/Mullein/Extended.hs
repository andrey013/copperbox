{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    printGlyph


  -- * Notes with fingering annotations
  -- $finger_anno
  , FingerNumber
  , HasFingerNumber
  , FingeredGlyph
  , FingeredGlyph'
  , lyFingeredGlyph
  
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

  -- * Tab string number
  -- $tab_string_anno
  , HasStringNumber(..)
  , lyTabGlyph
  , TabGlyph
  , TabGlyph'
  , StringNumber

  ) where

import Mullein.Core
import Mullein.Duration
import Mullein.LilyPondDoc
import Mullein.LilyPondOutput
import Mullein.Pitch
import Mullein.Utils ( optDoc, mbDoc )

import Text.PrettyPrint.Leijen



printGlyph ::(anno -> Doc) -> Glyph anno Pitch (Maybe Duration) -> Doc
printGlyph f (Note a p d t)  = pitch p <> mbDoc duration d 
                                       <> f a <> optDoc t tie
printGlyph _ (Rest d)        = rest d
printGlyph _ (Spacer d)      = spacer d
printGlyph f (Chord ps d t)  = chordForm (map fn ps) d <> optDoc t tie
                               where fn (a,p) = pitch p <> f a
printGlyph f (GraceNotes xs) = graceForm $ map fn xs 
    where fn (GraceNote a p d) = pitch p <> mbDoc duration d <> f a


--------------------------------------------------------------------------------
-- Notes with fingering annotations - LilyPond only

-- $finger_anno
-- Note form is \<pitch\>\<duration\>/-/\<finger_number\> 
-- e.g. @c4-2@
--
-- Chord form is /</\<pitch\>/-/\<finger_number> .../>/\<duration\> 
-- e.g. @\<c-1 e-2 g-4>2@


newtype FingerNumber = FingerNumber Int
  deriving (Eq,Ord,Num)


instance Show FingerNumber where
  showsPrec p (FingerNumber i) = showsPrec p i


class HasFingerNumber anno where 
  getFingerNumber :: anno -> Maybe FingerNumber
  setFingerNumber :: FingerNumber -> anno -> anno
 
instance HasFingerNumber (Maybe FingerNumber) where
  getFingerNumber = id
  setFingerNumber = const . Just


-- Fingering is optional - numering every note would clutter up
-- scores, especially where notes are repeated.

type FingeredGlyph  = Glyph (Maybe FingerNumber) Pitch Duration
type FingeredGlyph' = Glyph (Maybe FingerNumber) Pitch (Maybe Duration)  


lyFingeredGlyph :: HasFingerNumber anno 
                => Glyph anno Pitch (Maybe Duration) -> Doc
lyFingeredGlyph = printGlyph (mbDoc fingerNumber . getFingerNumber)


fingerNumber :: FingerNumber -> Doc
fingerNumber (FingerNumber i) = char '-' <> int i



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

instance MakeSpacer SpacerGlyph where
  makeSpacer = SpacerMark Nothing

instance MakeRest SpacerGlyph where
  makeRest = makeSpacer


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

-- Should not need an ExtBeam instance as SpacerGlyphs should be 
-- partitioned to bars with @phraseNoPulses@.
--
-- instance ExtBeam (SpacerMark dur) where
--   outerElement (SpacerMark _ _)   = False


instance HasDuration SpacerMark where
  getDuration (SpacerMark _ d) = d


instance ChangeDurationLyRel SpacerMark where
  changeDurationLyRel d0 (SpacerMark a d) = (SpacerMark a (alterDuration d0 d), d)

--------------------------------------------------------------------------------
-- Tab Glyphs with string number


-- $tab_string_anno
-- Note form is \<pitch\>\<duration\>/\/\<finger_number\> 
-- e.g. @c4\\2@
--
-- Chord form is /</\<pitch\>/\/\<finger_number> .../>/\<duration\> 
-- e.g. @\<c\\1 e\\2 g\\4>2@

-- Notes are annotated with string number

newtype StringNumber = StringNumber Int
  deriving (Eq,Ord,Num)

instance Show StringNumber where
  showsPrec p (StringNumber i) = showsPrec p i

class HasStringNumber anno where
  getStringNumber :: anno -> StringNumber
  setStringNumber :: StringNumber -> anno -> anno

instance HasStringNumber StringNumber where
  getStringNumber = id
  setStringNumber = const

lyTabGlyph :: HasStringNumber anno => Glyph anno Pitch (Maybe Duration) -> Doc
lyTabGlyph = printGlyph (stringNumber . getStringNumber)

stringNumber :: StringNumber -> Doc
stringNumber (StringNumber i) = char '\\' <> int i



-- NOTE - these types/instances are due for removal.

type TabGlyph  = Glyph StringNumber Pitch Duration
type TabGlyph' = Glyph StringNumber Pitch (Maybe Duration)

