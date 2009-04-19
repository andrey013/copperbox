{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.ScoreDatatypes
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Score syntax tree
--
--------------------------------------------------------------------------------

module Mullein.ScoreDatatypes where

import Mullein.CoreTypes
import Mullein.Duration
import Mullein.Pitch



data Part e  = Part [Phrase e]
  deriving (Eq,Show)

data Phrase e = Phrase   (Motif e)
              | Repeated (Motif e)
              | FSRepeat (Motif e) (Motif e) (Motif e)
  deriving (Eq,Show)

data Motif e = Motif Key Meter [Bar e]
  deriving (Eq,Show)

data Bar e = Bar     (Unison e)
           | Overlay (Unison e) [Unison e]
  deriving (Eq,Show)


type Tied = Bool

data Unison e = Unison [Bracket e] Tied
  deriving (Eq,Show)


-- bracket together beamed notes to make a pulsation
data Bracket e = Singleton e
               | Bracket   [e]
  deriving (Eq,Show)


-- Element is the typical parameter for the Part syntax tree
-- but there can be others...

data Element = Note   Pitch     Duration
             | Rest   Duration
             | Spacer Duration
             | Chord  [Pitch]   Duration
             | GraceNotes [GraceNote]
  deriving (Eq,Show)
        
type GraceNote  = (Pitch, Duration)


--------------------------------------------------------------------------------
-- Note lists

type NoteList = [Element]


instance Temporal Element where 
  duration (Note _ d)             = d
  duration (Rest d)               = d
  duration (Spacer d)             = d
  duration (Chord _ d )           = d
  duration (GraceNotes _)         = 0
    
 
  
  swapDuration d (Note p _)       = Note p d
  swapDuration d (Rest _)         = Rest d
  swapDuration d (Spacer _)       = Spacer d
  swapDuration d (Chord se _)     = Chord se d
  swapDuration _ (GraceNotes se)  = GraceNotes se
                  
                  
                  
instance Spacer Element where
  spacer d = Spacer d

  
