{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.Datatypes
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Data types (e.g time signature) for musical structures.
--
--------------------------------------------------------------------------------

module Neume.Core.Datatypes
  (

  -- * Notelists
    NoteList
  , PletTree(..)
  , plet
  , duplet
  , triplet
  , simpleNoteList

   
  ) where

import Neume.Core.Metrical

import Text.PrettyPrint.Leijen          -- package: wl-pprint


-- | A 'NoteList' is a list of notes (or more properly glyphs as
-- it may contain rests etc.).
-- 
-- This is the initial data structure representing musical 
-- fragments. A NoteGroup is processed by Neume (split into bars
-- and beamed according to a 'MeterPattern'), then rendered to
-- ABC or LilyPond.
--
-- To handle n-ary tuplets a NoteGroup is unfortunately somewhat 
-- more complicated than a simple (linear) list.
--
type NoteList a = [PletTree a]


-- | A \PletTree\ represents an element in a 'NoteGroup'. A 
-- element may be either a single note (constructor S) or an 
-- n-ary tuplet (constructor Plet) which is recursive, so that 
-- tuplets can contain tuplets.
--
data PletTree a = S a                           -- Single \"note\"
                | Plet PletMult [PletTree a]
  deriving (Eq,Show)


instance Functor PletTree where
  fmap f (S a)        = S (f a)
  fmap f (Plet pm xs) = Plet pm (map (fmap f) xs)


-- | Short-hand constructor for n-ary plets.
--
plet :: Integer -> Integer -> [PletTree a] -> PletTree a
plet p q xs = Plet (p,q) xs 

-- | Create a duplet - two notes in the time of three.
--
duplet :: a -> a -> PletTree a 
duplet a b = plet 2 3 [S a,S b]

-- | Create a triplet - three notes in the time of two.
--
triplet :: a -> a -> a -> PletTree a 
triplet a b c = plet 3 2 [S a,S b,S c]


-- | Convert a linear list of notes / glyphs (i.e no tuplets 
-- or duplets) into a 'NoteList'.
--
simpleNoteList :: [a] -> NoteList a
simpleNoteList = map S


--------------------------------------------------------------------------------
-- Pretty instances

instance Pretty a => Pretty (PletTree a) where 
  pretty (S a)              = pretty a
  pretty (Plet (p,q) notes) = braces (pletm p q <+> pretty notes)
    where
      pletm n d = integer n <> colon <> integer d


