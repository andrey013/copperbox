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
import HNotate.Utils

import Data.Ratio
import Data.Sequence hiding ( length )
import qualified Data.Sequence as S

import Text.PrettyPrint.Leijen


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
        { chord_elements      :: [Pitch] 
        , rhythmic_value      :: Duration
        }          
    | GraceNotes 
        { grace_elements      :: [GraceNote] }                              
    | Nplet 
        { nplet_multipier     :: Int
        , unit_duration       :: Duration
        , nplet_elements      :: [Pitch] 
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

        
reunit :: Duration -> Int -> [a] -> Duration
reunit tot i xs = tot * (makeDuration l i) * (makeDuration 1 l) where
                    l = length xs 
                  
                  
                  
instance Spacer Element where
  spacer d = Spacer d

                  
npletDuration :: Int -> Duration -> Duration
npletDuration len unit_d = (fromIntegral len % 1) * unit_d   

               
note :: Pitch -> Duration -> NoteList -> NoteList
note p d t = (Note p d) `event` t

rest :: Duration -> NoteList -> NoteList
rest d t = (Rest d) `event` t

root :: NoteList
root = lineTree

poly              :: [NoteList] -> NoteList -> NoteList
poly xs t         = t |*> Overlay xs


collapseTree :: Temporal a => LineTree a -> [(Duration, Seq a)]
collapseTree = levelSt (\s e -> s + duration e) 0 


--------------------------------------------------------------------------------
-- pretty print

instance Pretty Element where
  pretty (Note p d)           = pretty p <> prime <> ppDuration d
  pretty (Rest d)             = char 'r' <> ppDuration d 
  pretty (Spacer d)           = char 's' <> ppDuration d
  pretty (Chord se d)         = brackets (hsep $ fmap pretty se) 
                                    <> prime <> ppDuration d
      
  pretty (GraceNotes se)      = braces (hsep $ fmap fn se) where 
                                      fn (p,d) = pretty p <> prime <> ppDuration d
    
  pretty (Nplet _ _ se)       = braces (hsep $ fmap pretty se)    

