{-# OPTIONS -Wall #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.AbcNoteClass
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Operations a /Note/ must support to be renderable with ABC.
--
--------------------------------------------------------------------------------

module Mullein.AbcNoteClass where

import Mullein.Duration
import Mullein.LabelSet

import Text.PrettyPrint.Leijen

class AbcNote e where
  -- | Abc prints pitches relative to the key signature rather than
  -- as absolute values, whereas Mullein (and LilyPond) represent 
  -- pitches as absolute values. 
  -- Notes are respelt during the conversion to Abc  
  respell  :: LabelSet -> e -> e

  -- (notes) ...
  abcNote  :: e -> Duration -> Doc
  
  -- (chords, graces) ...
  abcPitch :: e -> Doc
  
  -- | When a the Note representation includes annotations 
  -- (e.g. fingerings) these are printed inline.
  inlineAnno :: e -> Maybe Doc

