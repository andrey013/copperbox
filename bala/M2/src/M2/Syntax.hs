{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  M2.Syntax
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Music rep syntax tree.
--
--------------------------------------------------------------------------------

module M2.Syntax
  (
  -- * Phrases and bars
    Phrase(..)
  , Bar(..)

  , LyPhrase
  , LyBar
  , AbcPhrase
  , AbcBar

  , StaffPhrase
  , StaffBar

  , NonStdPhrase
  , NonStdBar

  -- * Staff expressions
  , BarUnit
  , CExpr(..)
  , N_PletDescr(..)
  , AExpr(..)


  -- * Staff glyphs
  , Glyph(..)
  , Note(..)
  , Tie
  , ChordPitch(..)

  -- * Skip expressions
  , SimpleBarUnit

  -- * Skip glyphs
  , SkipGlyph(..)  

  ) where


import M2.OneList

import Text.PrettyPrint.Leijen          -- package : wl-print


--------------------------------------------------------------------------------
-- Phrases and bars 

-- (Phrases and bars are composable with pretty-print operations...)

newtype Phrase a = Phrase { getPhrase :: [Bar a] }
newtype Bar    a = Bar    { getBar    :: a } 


type LyPhrase   = Phrase Doc
type LyBar      = Bar    Doc

type AbcPhrase  = Phrase Doc
type AbcBar     = Bar    Doc

type StaffPhrase anno pch dur = Phrase (BarUnit anno pch dur)
type StaffBar    anno pch dur = Bar    (BarUnit anno pch dur) 


type NonStdPhrase glyph dur   = Phrase (SimpleBarUnit glyph dur)
type NonStdBar    glyph dur   = Bar    (SimpleBarUnit glyph dur)


--------------------------------------------------------------------------------
-- Staff \Expressions\

-- Note - AExprs (atomic expressions) make a distinction between 
-- glyphs (notes, chords, rests) and grace notes ([note]), so 
-- they are fixed to Staff glyphs rather than parametric on some 
-- \glyph\.


type BarUnit anno pch dur = OneList (CExpr anno pch dur)


-- | Contextual expression. This is a sequence of one or more 
-- notes together with some context to be communicated to the 
-- pretty printer - the context being either that the notes 
-- should be beamed or that they are n-plets (duplets, triplets, 
-- ...). 
--
-- Note this formulation permits beam groups within beam groups.
-- Ideally this would be disallowed, but beam groups may contain
-- n-plets (and n-plets must be recursive).
--

data CExpr anno pch dur = Atomic  (OneList (AExpr anno pch dur)) 
                        | N_Plet  N_PletDescr  (CExpr anno pch dur)
                        | Beamed  (CExpr anno pch dur)
  deriving (Eq,Show)

data N_PletDescr = N_PletDescr Int Int
  deriving (Eq,Show) 


-- Atomic expression - considered to have a single duration.
--
-- Technically graces have no duration (when segmenting 
-- bars), but so the grace can be printed each gracenote has
-- its own duration.
 
data AExpr anno pch dur = Glyph (Glyph anno pch dur)
                        | Grace (OneList (Note anno pch dur))
   deriving (Eq,Show) 


--------------------------------------------------------------------------------
-- Staff Glyphs


data Glyph anno pch dur = GlyNote  (Note anno pch dur) !Tie
                        | Rest     !dur
                        | Spacer   !dur
                        | Chord    (OneList (ChordPitch anno pch)) !dur !Tie
  deriving (Eq,Show)


data Note anno pch dur = Note !anno !pch !dur
  deriving (Eq,Show) 

type Tie = Bool

-- Note dur is not used... 
data ChordPitch anno pch = ChordPitch !anno !pch
  deriving (Eq,Show)


--------------------------------------------------------------------------------
-- Simple Expressions

-- These are used to print non-standard glyphs 
-- (e.g chord diagrams) above the staff.

type SimpleBarUnit glyph dur = OneList (SkipGlyph glyph dur)



--------------------------------------------------------------------------------
-- \Skip Glyphs\

-- For LilyPond...

data SkipGlyph glyph dur = SGlyph   glyph   !dur
                         | Skip     !dur
  deriving (Eq,Show)






