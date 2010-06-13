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
  , PhraseName
  , Bar

  -- * Metrical divisions of a bar - beam groups, tuplets, ... 
  , SMetricalDiv(..)
  , MetricalDiv(..)
  , PletMult                    -- .................................
  , atom 
  , n_plet
  , beamed

  -- * Note lists
  , NoteList(..)
  , SDivision(..)
  , Division(..)
  , oneElem
  , plet
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

import Neume.Core.Utils.OneList

import Text.PrettyPrint.Leijen hiding ( (<$>) )   -- package: wl-pprint

import Control.Applicative
import Data.Foldable
import Data.Traversable

-- | Phrase formats

newtype Full      gly = Full      (Phrase   (Bar     (MetricalDiv gly)))
newtype Undiv     gly = Undiv     (Phrase   (Bar                  gly))
newtype Unmetered gly = Unmetered (Phrase            (MetricalDiv gly))




-- Note - Phrase isn\'t recursive... 

data Phrase e = Phrase 
      { phrase_name   :: PhraseName
      , phrase_bars   :: [e]
      }
  deriving (Show)

type PhraseName     = String

type Bar e = [e]



--------------------------------------------------------------------------------
-- Metrical division - note groups - maybe beamed, tuplets... 

-- | 'SMetricalDiv' - structure datatype for note groups (metrical 
-- divisions of a bar) - beamed, tuplets, or single notes.
--
-- Beaming is likely to be synthesized by Neume, rather than
-- constructed directly. 
--
data SMetricalDiv x e = Atom e
                      | N_Plet PletMult [x]
                      | Beamed          [x]
  deriving (Show)

type PletMult = (Integer, Integer)

-- | 'NoteGroup' - recursive wrapper for SNoteGroup.
--
newtype MetricalDiv e = WrapMD (SMetricalDiv (MetricalDiv e) e)
  deriving (Show)

atom            :: e -> MetricalDiv e
atom e          = WrapMD $ Atom e

n_plet          :: PletMult -> [MetricalDiv e] -> MetricalDiv e
n_plet mul xs   = WrapMD $ N_Plet mul xs

beamed          :: [MetricalDiv e] -> MetricalDiv e
beamed xs       = WrapMD $ Beamed xs

--------------------------------------------------------------------------------
-- Note lists - created by the user...

data NoteList e = NoteList
      { note_list_name  :: String
      , note_list_notes :: [e]
      }

data SDivision x e = OneElem e
                   | Plet PletMult [x]
  deriving (Show)

newtype Division e = WrapDiv (SDivision (Division e) e)
  deriving (Show)


oneElem         :: e -> Division e
oneElem e       = WrapDiv $ OneElem e


plet            :: PletMult -> [Division e] -> Division e
plet mul xs     = WrapDiv $ Plet mul xs


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
  fmap f (Phrase name bars) = Phrase name $ map f bars

instance Functor MetricalDiv where
  fmap f (WrapMD (Atom e))        = atom (f e)
  fmap f (WrapMD (N_Plet mul xs)) = n_plet mul (map (fmap f) xs)
  fmap f (WrapMD (Beamed xs))     = beamed (map (fmap f) xs)

instance Foldable MetricalDiv where
  foldMap f (WrapMD (Atom e))        = f e
  foldMap f (WrapMD (N_Plet _ xs))   = foldMap (foldMap f) xs
  foldMap f (WrapMD (Beamed xs))     = foldMap (foldMap f) xs

instance Traversable MetricalDiv where
  traverse f (WrapMD (Atom e))      = atom <$> f e
  traverse f (WrapMD (N_Plet m xs)) = (n_plet m) <$> traverse (traverse f) xs
  traverse f (WrapMD (Beamed xs))   = beamed <$> traverse (traverse f) xs


