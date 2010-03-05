{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.SyntaxMarkup
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

module Neume.Core.SyntaxMarkup
  (
  -- * Phrases and bars
    MarkupPhrase(..)
  , MarkupBar(..)

  -- * Skip glyphs
  , SkipGlyph(..)  

  ) where

import Neume.Core.Duration
import Neume.Core.Utils.OneList
import Neume.Core.Utils.StateMap

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



-- StateMap

instance StateMap MarkupPhrase where
  stmap f (MarkupPhrase xs) st = (MarkupPhrase xs',st') 
                                 where (xs',st') = stmap (stmap f) xs st

instance StateMap MarkupBar where
  stmap f (MarkupBar os) st = (MarkupBar os',st') 
                              where (os',st') = stmap f os st

-- StateMap2

instance StateMap2 SkipGlyph where
  stmap2 f g (SGlyph gly d) st = (SGlyph gly' d',st'') 
                                 where (gly',st')  = f gly st
                                       (d',st'')   = g d st'

  stmap2 _ g (Skip d)       st = (Skip d',st') where (d',st') = g d st

--------------------------------------------------------------------------------
-- Spacer

instance MakeSpacer (SkipGlyph gly Duration) where
  makeSpacer d = Skip d
