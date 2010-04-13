{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.SyntaxGlyph
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Music syntax tree for staff notation.
--
--------------------------------------------------------------------------------

module Neume.Core.SyntaxGlyph
  (

  -- * Staff glyphs
    Glyph(..)
  , Note(..)
  , Tie
  , ChordPitch(..)
  
  -- * Markup glyphs for fret diags etc.
  , MarkupGlyph(..)

  -- * Synonyms

  , GlyphDur
  , NoteDur

  , GlyphRelDur
  , NoteRelDur

  , StdGlyph
  , AnnoGlyph

  ) where


import Neume.Core.Duration
import Neume.Core.Metrical
import Neume.Core.Pitch
import Neume.Core.Utils
import Neume.Core.Utils.OneList

import Text.PrettyPrint.Leijen          -- package: wl-pprint

--------------------------------------------------------------------------------
-- Staff Glyphs



data Glyph anno pch dur = GlyNote  (Note anno pch dur) !Tie
                        | Rest     !dur
                        | Spacer   !dur
                        | Chord    (OneList (ChordPitch anno pch)) !dur !Tie
                        | Graces   (OneList (Note anno pch dur)) 
  deriving (Eq,Show)


data Note anno pch dur = Note !anno !pch !dur
  deriving (Eq)

type Tie = Bool

-- Note dur is not used... 
data ChordPitch anno pch = ChordPitch !anno !pch
  deriving (Eq,Show)




--------------------------------------------------------------------------------
-- \Skip Glyphs\

-- For LilyPond...

data MarkupGlyph glyph dur = MGlyph   glyph   !dur
                           | Skip     !dur
  deriving (Eq,Show)


--------------------------------------------------------------------------------

-- Synonyms


type GlyphDur    anno pch   = Glyph anno pch Duration
type NoteDur     anno pch   = Note  anno pch Duration


-- | LilyPond shorthand...
type GlyphRelDur anno pch   = Glyph anno pch (Maybe Duration)
type NoteRelDur  anno pch   = Note  anno pch (Maybe Duration)



type StdGlyph           = Glyph ()   Pitch Duration
type AnnoGlyph anno     = Glyph anno Pitch Duration



-- FMap2 

instance FMap2 ChordPitch where
  fmap2 f g (ChordPitch a p) = ChordPitch (f a) (g p)

-- FMap3
instance FMap3 Note where
  fmap3 f1 f2 f3 (Note a p d) = Note (f1 a) (f2 p) (f3 d)


instance FMap3 Glyph where
  fmap3 f1 f2 f3 (GlyNote n t)  = GlyNote (fmap3 f1 f2 f3 n) t
  fmap3 _  _  f3 (Rest d)       = Rest (f3 d)
  fmap3 _  _  f3 (Spacer d)     = Spacer (f3 d)
  fmap3 f1 f2 f3 (Chord os d t) = Chord (fmap (fmap2 f1 f2) os) (f3 d) t
  fmap3 f1 f2 f3 (Graces os)    = Graces (fmap (fmap3 f1 f2 f3) os)


-- StateMap2 

instance StateMap2 ChordPitch where
  stmap2 f g st (ChordPitch a p) = stBinary ChordPitch f g st a p


-- StateMap3 

instance StateMap3 Note where
  stmap3 f1 f2 f3 st (Note a p d) = stTernary Note f1 f2 f3 st a p d


instance StateMap3 Glyph where
  stmap3 f1 f2 f3 st (GlyNote n t)  = 
    fmap2a (\nt -> GlyNote nt t) $ stmap3 f1 f2 f3 st n

  stmap3 _  _  f3 st (Rest d)       = fmap2a Rest  $ f3 st d
  stmap3 _  _  f3 st (Spacer d)     = fmap2a Spacer $ f3 st d
  stmap3 f1 f2 f3 st (Chord os d t) = 
    stBinary (\xs drn -> Chord xs drn t) (stmap (stmap2 f1 f2)) f3 st os d 
  
  stmap3 f1 f2 f3 st (Graces os)    = 
    fmap2a Graces $ stmap (stmap3 f1 f2 f3) st os


--------------------------------------------------------------------------------

instance DMeasure (Glyph anno pch Duration) where
  dmeasure (GlyNote  (Note _ _ d) _) = dmeasure d
  dmeasure (Rest     d)              = dmeasure d
  dmeasure (Spacer   d)              = dmeasure d
  dmeasure (Chord _ d _)             = dmeasure d
  dmeasure (Graces _)                = 0

instance MakeSpacer (Glyph anno pch Duration) where
  makeSpacer d = Spacer d

instance MakeRest (Glyph anno pch Duration) where
  makeRest d = Rest d


--------------------------------------------------------------------------------
-- Spacer

instance MakeSpacer (MarkupGlyph gly Duration) where
  makeSpacer d = Skip d

--------------------------------------------------------------------------------
-- NumMeasured

instance DMeasure (MarkupGlyph gly Duration) where
  dmeasure (MGlyph _ d) = dmeasure d
  dmeasure (Skip     d) = dmeasure d



--------------------------------------------------------------------------------
instance BeamExtremity (Glyph anno pch dur) where
  rendersToNote (GlyNote _ _) = True
  rendersToNote (Rest _)      = False
  rendersToNote (Spacer _)    = False
  rendersToNote (Chord _ _ _) = True
  rendersToNote (Graces _)    = False
 

--------------------------------------------------------------------------------
-- Show instances

instance (Show pch, ShowsDuration dur) => Show (Note anno pch dur) where
  showsPrec _ (Note _ pch dur) = shows pch . showsDur dur

-- Pretty instances

snocDur :: Pretty a => Doc -> a -> Doc
snocDur d drn = d <> char '\'' <> pretty drn

snocTie :: Doc -> Bool -> Doc
snocTie d True = d <> char '~'
snocTie d _    = d


instance (Pretty pch, Pretty dur) => Pretty (Glyph anno pch dur) where
  pretty (GlyNote n t)  = pretty n `snocTie` t
  pretty (Rest    d)    = char 'r' `snocDur` d
  pretty (Spacer  d)    = char 's' `snocDur` d
  pretty (Chord os d t) = (angles $ hsep $ toListF pretty os) `snocDur` d 
                                                              `snocTie` t

  pretty (Graces os)    = braces $ hsep $ toListF pretty os


instance (Pretty pch, Pretty dur) => Pretty (Note anno pch dur) where
  pretty (Note _ p d) = pretty p `snocDur` d


instance (Pretty pch) => Pretty (ChordPitch anno pch) where
  pretty (ChordPitch _ p) = pretty p

