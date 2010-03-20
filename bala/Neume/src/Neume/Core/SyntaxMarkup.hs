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
  stmap f st (MarkupPhrase xs) = (MarkupPhrase xs',st') 
                                 where (xs',st') = stmap (stmap f) st xs

instance StateMap MarkupBar where
  stmap f st (MarkupBar os) = (MarkupBar os',st') 
                              where (os',st') = stmap f st os

-- StateMap2

instance StateMap2 SkipGlyph where
  stmap2 f g st (SGlyph gly d) = (SGlyph gly' d',st'') 
                                 where (gly',st')  = f st gly
                                       (d',st'')   = g st' d

  stmap2 _ g st (Skip d)       = (Skip d',st') where (d',st') = g st d

--------------------------------------------------------------------------------
-- Spacer

instance MakeSpacer (SkipGlyph gly Duration) where
  makeSpacer d = Skip d
