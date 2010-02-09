{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.SyntaxMarkup
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Music syntax tree for notation or markup to be printed above 
-- the staff - e.g. Chord diagrams.
--
-- Markup is much more limited than staff syntax - it does not
-- support grace notes, chords, beam groups, tuplets. 
--
--------------------------------------------------------------------------------

module Neume.SyntaxMarkup
  (
  -- * Phrases and bars
    MarkupPhrase(..)
  , MarkupBar(..)

  -- * Skip glyphs
  , SkipGlyph(..)  

  ) where


import Neume.OneList


--------------------------------------------------------------------------------
-- Phrases and bars 

-- Note - phrases and bars are polymorphic on the glyph type. 
-- They can use alternatives to the SkipGlyph type. 

newtype MarkupPhrase gly = MarkupPhrase { getMarkupPhrase :: [MarkupBar gly] }
newtype MarkupBar    gly = MarkupBar    { getMarkupBar    :: OneList gly } 



--------------------------------------------------------------------------------
-- \Skip Glyphs\

-- For LilyPond...

data SkipGlyph glyph dur = SGlyph   glyph   !dur
                         | Skip     !dur
  deriving (Eq,Show)


