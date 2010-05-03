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
  , GraceNote(..)
  
  , chord 
    
  -- * Markup glyphs for fret diags etc.
  , MarkupGlyph(..)

  -- * Synonyms

  , GlyphDur

  , GlyphRelDur

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



data Glyph anno pch dur = GlyNote  (Note anno pch) !dur !Tie
                        | Rest     !dur
                        | Spacer   !dur
                        | Chord    (OneList (Note anno pch)) !dur !Tie
                        | Graces   (OneList (GraceNote anno pch dur)) 
  deriving (Eq,Show)


data Note anno pch = Note !anno !pch
  deriving (Eq)

type Tie = Bool

-- Unfortunately Grace notes have funny semantics vis-a-vis 
-- duration...

data GraceNote anno pch dur = GraceNote !anno !pch !dur
  deriving (Eq,Show)


-- Wrapper constructor - avoids exposing OneList
chord :: [Note anno pch] -> dur -> Tie -> Glyph anno pch dur
chord [] _ _ = error "chord - cannot create a chord from the empty list."
chord xs d t = Chord (fromList xs) d t


--------------------------------------------------------------------------------
-- \Skip Glyphs\

-- For LilyPond...

data MarkupGlyph glyph dur = MGlyph   glyph   !dur
                           | Skip     !dur
  deriving (Eq,Show)


--------------------------------------------------------------------------------

-- Synonyms


type GlyphDur    anno pch   = Glyph anno pch Duration


-- | LilyPond shorthand...
type GlyphRelDur anno pch   = Glyph anno pch (Maybe Duration)



type StdGlyph           = Glyph ()   Pitch Duration
type AnnoGlyph anno     = Glyph anno Pitch Duration



-- FMap2 

instance FMap2 Note where
  fmap2 f g (Note a p) = Note (f a) (g p)

-- FMap3
instance FMap3 GraceNote where
  fmap3 f1 f2 f3 (GraceNote a p d) = GraceNote (f1 a) (f2 p) (f3 d)


instance FMap3 Glyph where
  fmap3 f1 f2 f3 (GlyNote n d t)  = GlyNote (fmap2 f1 f2 n) (f3 d) t
  fmap3 _  _  f3 (Rest d)         = Rest (f3 d)
  fmap3 _  _  f3 (Spacer d)       = Spacer (f3 d)
  fmap3 f1 f2 f3 (Chord os d t)   = Chord (fmap (fmap2 f1 f2) os) (f3 d) t
  fmap3 f1 f2 f3 (Graces os)      = Graces (fmap (fmap3 f1 f2 f3) os)


-- StateMap2 

instance StateMap2 Note where
  stmap2 f g st (Note a p) = stBinary Note f g st a p


-- StateMap3 

instance StateMap3 GraceNote where
  stmap3 f1 f2 f3 st (GraceNote a p d) = stTernary GraceNote f1 f2 f3 st a p d


instance StateMap3 Glyph where
  stmap3 f1 f2 f3 st (GlyNote n d t)  = 
    stBinary (\nt drn -> GlyNote nt drn t) (stmap2 f1 f2) f3 st n d

  stmap3 _  _  f3 st (Rest d)         = fmap2a Rest   $ f3 st d
  stmap3 _  _  f3 st (Spacer d)       = fmap2a Spacer $ f3 st d
  stmap3 f1 f2 f3 st (Chord os d t)   = 
    stBinary (\xs drn -> Chord xs drn t) (stmap (stmap2 f1 f2)) f3 st os d 
  
  stmap3 f1 f2 f3 st (Graces os)    = 
    fmap2a Graces $ stmap (stmap3 f1 f2 f3) st os


--------------------------------------------------------------------------------
-- DMeasure

instance DMeasure (Glyph anno pch Duration) where
  dmeasure (GlyNote _ d _)    = dmeasure d
  dmeasure (Rest     d)       = dmeasure d
  dmeasure (Spacer   d)       = dmeasure d
  dmeasure (Chord _ d _)      = dmeasure d
  dmeasure (Graces _)         = 0

instance DMeasure (MarkupGlyph gly Duration) where
  dmeasure (MGlyph _ d) = dmeasure d
  dmeasure (Skip     d) = dmeasure d



--------------------------------------------------------------------------------
-- Spacer


instance MakeSpacer (Glyph anno pch Duration) where
  makeSpacer d = Spacer d

instance MakeRest (Glyph anno pch Duration) where
  makeRest d = Rest d

instance MakeSpacer (MarkupGlyph gly Duration) where
  makeSpacer d = Skip d

--------------------------------------------------------------------------------
-- Beam Extremity


instance BeamExtremity (Glyph anno pch dur) where
  rendersToNote (GlyNote _ _ _) = True
  rendersToNote (Rest _)        = False
  rendersToNote (Spacer _)      = False
  rendersToNote (Chord _ _ _)   = True
  rendersToNote (Graces _)      = False

instance BeamExtremity (MarkupGlyph gly dur) where
  rendersToNote (MGlyph _ _)  = True
  rendersToNote (Skip _)      = False
 

--------------------------------------------------------------------------------
-- Show instances

instance (Show pch) => Show (Note anno pch) where
  showsPrec _ (Note _ pch) = shows pch

-- Pretty instances

snocDur :: Pretty a => Doc -> a -> Doc
snocDur d drn = d <> char '\'' <> pretty drn

snocTie :: Doc -> Bool -> Doc
snocTie d True = d <> char '~'
snocTie d _    = d


instance (Pretty pch, Pretty dur) => Pretty (Glyph anno pch dur) where
  pretty (GlyNote n d t) = pretty n `snocDur` d `snocTie` t
  pretty (Rest    d)     = char 'r' `snocDur` d
  pretty (Spacer  d)     = char 's' `snocDur` d
  pretty (Chord os d t)  = (angles $ hsep $ toListF pretty os) `snocDur` d 
                                                               `snocTie` t

  pretty (Graces os)    = braces $ hsep $ toListF pretty os


instance (Pretty pch) => Pretty (Note anno pch) where
  pretty (Note _ p) = pretty p


instance (Pretty pch, Pretty dur) => Pretty (GraceNote anno pch dur) where
  pretty (GraceNote _ p d) = pretty p `snocDur` d

