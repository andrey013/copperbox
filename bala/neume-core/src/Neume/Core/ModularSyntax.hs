{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.ModularSyntax
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Images - scores that have been partially rendered and are 
-- composed as Docs.
--
--
--------------------------------------------------------------------------------

module Neume.Core.ModularSyntax
  (

  -- * Phrase formats
    Full(..)
  , Undiv(..)
  , Unmetered(..)
  
  -- * Phrase
  , Phrase(..)
  , Bar

  -- * Metrical divisions of a bar - beam groups, tuplets, ... 
  , MetricalDiv(..)

  -- * Note lists
  , NoteList(..)
  , Division(..)
  , DivisionNoteList
  , SimpleNoteList

  -- * Glyphs
  , Glyph(..)
  , Note(..)
  , Tie(..)
  , GraceNote(..)

  -- * Graphics
  , Graphic(..)

  -- * Image
  , BarImage
  , PhraseImage
  , PhraseOverlayImage

  ) where

import Neume.Core.Duration
import Neume.Core.Metrical
import Neume.Core.Utils.OneList

import Text.PrettyPrint.Leijen hiding ( (<$>) )   -- package: wl-pprint

import Control.Applicative
import Data.Foldable
import Data.Traversable

-- | Phrase formats

newtype Full      gly = Full      (Phrase   (Bar     (MetricalDiv gly)))
newtype Undiv     gly = Undiv     (Phrase   (Bar                  gly))
newtype Unmetered gly = Unmetered (Phrase            (MetricalDiv gly))





newtype Phrase e = Phrase { getPhraseBars :: [e] } 
  deriving (Show)


type Bar e = [e]



--------------------------------------------------------------------------------
-- Metrical division - note groups - maybe beamed, tuplets... 

-- | 'SMetricalDiv' - structure datatype for note groups (metrical 
-- divisions of a bar) - beamed, tuplets, or single notes.
--
-- Beaming is likely to be synthesized by Neume, rather than
-- constructed directly. 
--
data MetricalDiv e = Atom e
                   | Beamed          [MetricalDiv e]
                   | N_Plet PletMult [MetricalDiv e]
  deriving (Show)


--------------------------------------------------------------------------------
-- Note lists - created by the user...

newtype NoteList e = NoteList { getNotes :: [e] }
  deriving (Show)

data Division e = Unit e
                | Plet PletMult [Division e]
  deriving (Show)


type DivisionNoteList e = NoteList (Division e)
type SimpleNoteList   e = NoteList e


--------------------------------------------------------------------------------
-- Glyphs

-- Note - no recursion in Glyph so no Stype.
--
data Glyph anno pch dur = GlyNote  (Note anno pch) !dur !Tie
                        | Rest     !dur
                        | Spacer   !dur
                        | Chord    (OneList (Note anno pch)) !dur !Tie
                        | Graces   (OneList (GraceNote anno pch dur)) 
  deriving (Eq,Show)


data Note anno pch = Note !anno !pch
  deriving (Eq,Show)

data Tie = Tie | NoTie
  deriving (Eq,Show)

-- | Unfortunately Grace notes have funny semantics vis-a-vis 
-- duration:
--
-- Each grace note needs a duration for printing, but the 
-- durations are ignored by the processing steps (e.g beam
-- grouping).
--
data GraceNote anno pch dur = GraceNote !anno !pch !dur
  deriving (Eq,Show)



--------------------------------------------------------------------------------
-- Graphic - alternative glyph type

-- | For LilyPond fret diagrams etc. where there is either a
-- graphic or nothing (no grace notes, chords).
--
data Graphic gly dur = Graphic  gly   !dur
                     | Skip     !dur
  deriving (Eq,Show)



--------------------------------------------------------------------------------
-- Image

-- | 'Image' - a bar rendered to a Doc. 
-- 
-- After rendering to LilyPond or ABC notation, bars may 
-- still need some manipulation, before the full score is
-- output - e.g. grouping into overlays for polyphonic 
-- multi-voice music.
--
type BarImage = Doc

-- | A Phrase where all the bars have been rendered to images.
--
type PhraseImage = Phrase BarImage

-- | A Phrase with multiple, overlayed bars - e.g. for 
-- polyphonic music.
--
type PhraseOverlayImage = Phrase [BarImage]


--------------------------------------------------------------------------------

instance Functor Phrase where
  fmap f (Phrase bars) = Phrase $ map f bars

instance Functor MetricalDiv where
  fmap f (Atom e)        = Atom (f e)
  fmap f (Beamed xs)     = Beamed (map (fmap f) xs)
  fmap f (N_Plet mul xs) = N_Plet mul (map (fmap f) xs)

instance Foldable MetricalDiv where
  foldMap f (Atom e)        = f e
  foldMap f (Beamed xs)     = foldMap (foldMap f) xs
  foldMap f (N_Plet _ xs)   = foldMap (foldMap f) xs

instance Traversable MetricalDiv where
  traverse f (Atom e)      = Atom     <$> f e
  traverse f (Beamed xs)   = Beamed   <$> traverse (traverse f) xs
  traverse f (N_Plet m xs) = N_Plet m <$> traverse (traverse f) xs



--------------------------------------------------------------------------------

divisionFold :: (gly -> b -> b) -> (PletMult -> b -> b) -> b -> Division gly -> b
divisionFold f _ b (Unit a)     = f a b
divisionFold f g b (Plet pm xs) = foldl' (divisionFold f g) (g pm b) xs


-- | The measure of a \single\ or a \plet tree\ - plet trees are
-- considered indivisable so it is not a problem to sum them.
--

divisionMeasure :: DMeasure gly => Division gly -> DurationMeasure 
divisionMeasure = snd . divisionFold  phi chi (mult_stack_zero,0) where
  phi a  (stk,acc) = (stk, acc + nmeasureCtx stk a)
  chi pm (stk,acc) = (pushPM pm stk,acc) 


instance DMeasure gly => DMeasure (Division gly) where
  dmeasure = divisionMeasure

instance DMeasure (Glyph anno pch Duration) where
  dmeasure (GlyNote _ d _)    = dmeasure d
  dmeasure (Rest     d)       = dmeasure d
  dmeasure (Spacer   d)       = dmeasure d
  dmeasure (Chord _ d _)      = dmeasure d
  dmeasure (Graces _)         = 0

instance DMeasure (Graphic gly Duration) where
  dmeasure (Graphic _ d) = dmeasure d
  dmeasure (Skip     d)  = dmeasure d


instance BeamExtremity gly => BeamExtremity (Division gly) where
  rendersToNote (Unit a)       = rendersToNote a
  rendersToNote (Plet _ (x:_)) = rendersToNote x
  rendersToNote _              = False 


instance BeamExtremity (Glyph anno pch dur) where
  rendersToNote (GlyNote _ _ _) = True
  rendersToNote (Rest _)        = False
  rendersToNote (Spacer _)      = False
  rendersToNote (Chord _ _ _)   = True
  rendersToNote (Graces _)      = False

instance BeamExtremity (Graphic gly dur) where
  rendersToNote (Graphic _ _) = True
  rendersToNote (Skip _)      = False



