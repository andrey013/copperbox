{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.NoteList
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Building note lists
--
--
--------------------------------------------------------------------------------

module HNotate.NoteList where

import HNotate.Duration
import HNotate.LineTree
import HNotate.Pitch

import Data.Ratio
import Data.Sequence hiding ( length )
import qualified Data.Sequence as S

-- The Element datatype - represents elements with a 'unit duration'.
-- E.g a chord has a set of pitches but the unit duration is common to all 
-- of them. 
data Element = 
      Note 
        { note_pitch          :: Pitch
        , elt_duration        :: Duration
        }                  
    | Rest  
        { elt_duration        :: Duration }
    | Spacer  
        { elt_duration        :: Duration }
    | Chord 
        { chord_elements      :: Seq Pitch 
        , rhythmic_value      :: Duration
        }          
    | GraceNotes 
        { grace_elements      :: Seq GraceNote }                              
    | Nplet 
        { nplet_multipier     :: Int
        , unit_duration       :: Duration
        , nplet_elements      :: Seq Pitch 
        }                   
  deriving (Show) 

type GraceNote = (Pitch,Duration)

type NoteList = LineTree Element


instance Temporal Element where 
  duration (Note _ d)             = d
  duration (Rest d)               = d
  duration (Spacer d)             = d
  duration (Chord _ d )           = d
  duration (GraceNotes _)         = duration_zero
  duration (Nplet i d _)          = npletDuration i d
 
  
  swapDuration d (Note p _)       = Note p d
  swapDuration d (Rest _)         = Rest d
  swapDuration d (Spacer _)       = Spacer d
  swapDuration d (Chord se _)     = Chord se d
  swapDuration _ (GraceNotes se)  = GraceNotes se
  swapDuration d (Nplet i _ se)   = Nplet i ud se
    where ud = reunit d i se
        
reunit :: Duration -> Int -> Seq a -> Duration
reunit tot i se = let l = S.length se in 
                  tot * (makeDuration l i) * (makeDuration 1 l)
                  
npletDuration :: Int -> Duration -> Duration
npletDuration len unit_d = (fromIntegral len % 1) * unit_d   

               
note :: Pitch -> Duration -> NoteList -> NoteList
note p d t = (Note p d) `event` t

rest :: Duration -> NoteList -> NoteList
rest d t = (Rest d) `event` t

root :: NoteList
root = lineTree

collapseTree :: NoteList -> [(Duration, Seq Element)]
collapseTree = levelSt (\s e -> s + duration e) 0 

