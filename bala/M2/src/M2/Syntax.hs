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
    Phrase(..)
  , Bar(..)
  , VoiceUnit
  , CExpr(..)   
  , N_PletDescr(..)
  , AExpr(..)
  , Glyph(..)
  , Note(..)
  , Tie
  , ChordPitch(..)

  , StdNote
  , StdChordPitch
  , StdGlyph

  ) where


import M2.Duration
import M2.OneList
import M2.Pitch




newtype Phrase anno pch dur = Phrase { getPhrase :: [Bar anno pch dur] }


data Bar anno pch dur = Bar      (VoiceUnit anno pch dur)
                      | Overlays (OneList (VoiceUnit anno pch dur))
  deriving (Eq,Show)


type VoiceUnit anno pch dur = OneList (CExpr anno pch dur)



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



data Glyph anno pch dur = GlyNote  (Note anno pch dur) !Tie
                        | Rest     !dur
                        | Spacer   !dur
                        | Chord    (OneList (ChordPitch anno pch dur)) !dur !Tie
  deriving (Eq,Show)


data Note anno pch dur = Note !anno !pch !dur
  deriving (Eq,Show) 

type Tie = Bool

-- Note dur is not used... 
data ChordPitch anno pch dur = ChordPitch !anno !pch
  deriving (Eq,Show)



type StdGlyph      = Glyph      ()  Pitch  Duration 
type StdNote       = Note       ()  Pitch  Duration
type StdChordPitch = ChordPitch ()  Pitch  Duration


