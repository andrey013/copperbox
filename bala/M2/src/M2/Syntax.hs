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
    Phrase
  , Bar(..)
  , VoiceUnit
  , CExpr(..)   
  , N_PletDescr(..)
  , AExpr(..)
  , Glyph(..)
  , Note(..)
  , Tie


  , StdNote
  , StdChordPitch
  , StdGlyph

  ) where


import M2.Duration
import M2.OneList
import M2.Pitch




type Phrase anno pch dur = [Bar anno pch dur]


data Bar anno pch dur = Bar      (VoiceUnit anno pch dur)
                      | Overlays (OneList (VoiceUnit anno pch dur))
  deriving (Eq,Show)


type VoiceUnit anno pch dur = OneList (CExpr anno pch dur)



-- | Contextual expression. This is a sequence of one or more 
-- notes together with some context to be communicated to the 
-- pretty printer - the context being either that the notes 
-- should be within a beam group or they are n-plets (duplets, 
-- triplets, ...) so the metrical calculation is changed. 
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
                        | Grace [Note anno pch dur]
   deriving (Eq,Show) 



data Glyph anno pch dur = GlyNote  (Note anno pch dur)
                        | Rest     !dur
                        | Spacer   !dur
                        | Chord    [ChordPitch anno pch] !dur
  deriving (Eq,Show)


data Note anno pch dur = Note !anno !pch !dur !Tie
  deriving (Eq,Show) 


type Tie = Bool


data ChordPitch anno pch = ChordPitch !anno !pch
  deriving (Eq,Show)



type StdGlyph      = Glyph      ()  Pitch  Duration 
type StdNote       = Note       ()  Pitch  Duration
type StdChordPitch = ChordPitch ()  Pitch 


