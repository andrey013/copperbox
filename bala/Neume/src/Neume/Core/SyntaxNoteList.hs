{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.SyntaxNoteList
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Input syntax of note lists that allows nested tuplets.
--
--
--------------------------------------------------------------------------------

module Neume.Core.SyntaxNoteList
  (

  -- * Notelists
    NoteList(..)
  , PletForest
  , PletTree(..)
  , plet
  , duplet
  , triplet
  , simpleNoteList

  , pletFold
  , pletAll
  , pletMeasure
  , pletCount
   
  ) where

import Neume.Core.Duration
import Neume.Core.Metrical
import Neume.Core.Utils.StateMap

import Text.PrettyPrint.Leijen          -- package: wl-pprint

import Data.List ( foldl' )


-- | A 'NoteList' is a named list of notes, or more properly 
-- glyphs as it may contain rests etc. Furthermore it is 
-- actually a tree - so it can handle n-ary tuplets. 
-- 
-- This is the initial data structure representing musical 
-- fragments. A NoteGroup is processed by Neume (split into bars
-- and beamed according to a 'MeterPattern'), then rendered to
-- ABC or LilyPond.

data NoteList gly = NoteList 
      { note_list_name        :: String
      , note_list_notes       :: (PletForest gly)
      }
  deriving  (Eq,Show)

type PletForest gly = [PletTree gly]

-- | A \PletTree\ represents an element in a 'NoteGroup'. A 
-- element may be either a single note (constructor S) or an 
-- n-ary tuplet (constructor Plet) which is recursive, so that 
-- tuplets can contain tuplets.
--
data PletTree gly = S gly                         -- Single \"note\"
                  | Plet PletMult [PletTree gly]
  deriving (Eq,Show)


instance Functor PletTree where
  fmap f (S a)        = S (f a)
  fmap f (Plet pm xs) = Plet pm (map (fmap f) xs)

instance StateMap PletTree where
  stmap f st (S a)        = (S a',st') where (a',st') = f st a
  stmap f st (Plet pm xs) = (Plet pm xs',st') 
                            where (xs',st') = stmap (stmap f) st xs

-- | Short-hand constructor for n-ary plets.
--
plet :: Integer -> Integer -> [PletTree gly] -> PletTree gly
plet p q xs = Plet (p,q) xs 

-- | Create a duplet - two notes in the time of three.
--
duplet :: gly -> gly -> PletTree gly
duplet a b = plet 2 3 [S a,S b]

-- | Create a triplet - three notes in the time of two.
--
triplet :: gly -> gly -> gly -> PletTree gly
triplet a b c = plet 3 2 [S a,S b,S c]


-- | Convert a linear list of notes / glyphs (i.e no tuplets 
-- or duplets) into a 'NoteList'.
--
simpleNoteList :: (String,[gly]) -> NoteList gly
simpleNoteList (name,xs) = NoteList name (map S xs)



--------------------------------------------------------------------------------

pletFold :: (gly -> b -> b) -> (PletMult -> b -> b) -> b -> PletTree gly -> b
pletFold f _ b (S a)        = f a b
pletFold f g b (Plet pm xs) = foldl' (pletFold f g) (g pm b) xs


pletAll :: (gly -> Bool) -> PletTree gly -> Bool
pletAll test (S a)          = test a
pletAll test (Plet _ notes) = step notes where
   step []                      = True
   step (p:ps) | pletAll test p = step ps
   step _                       = False


--------------------------------------------------------------------------------
-- Measuring the plet-tree, and tuplet stack


-- | The measure of a \single\ or a \plet tree\ - plet trees are
-- considered indivisable so it is not a problem to sum them.
--
pletMeasure :: (Measurement gly ~ DurationMeasure, NumMeasured gly) 
            => PletTree gly -> DurationMeasure
pletMeasure = snd . pletFold  phi chi (mult_stack_zero,0) where
  phi a  (stk,acc) = (stk, acc + nmeasureCtx stk a)
  chi pm (stk,acc) = (pushPM pm stk,acc) 



--------------------------------------------------------------------------------

-- | The number of items in a PletTree 
--
-- NOTE - need the pred to test e.g. grace notes, which 
-- shouldn\'t be counted.
--
-- Is this a suitable case for another Type Class?
--
pletCount :: (gly -> Bool) -> PletTree gly -> Int
pletCount test = pletFold phi chi 0 where
  phi a n  | test a    = n+1
           | otherwise = n
  chi _ n              = n


--------------------------------------------------------------------------------
-- Pretty instances

instance Pretty gly => Pretty (PletTree gly) where 
  pretty (S a)              = pretty a
  pretty (Plet (p,q) notes) = braces (pletm p q <+> pretty notes)
    where
      pletm n d = integer n <> colon <> integer d


