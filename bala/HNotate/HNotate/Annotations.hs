--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Annotations
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Common annotations for LilyPond and Abc 
--
--------------------------------------------------------------------------------

module HNotate.Annotations where

import HNotate.Document
import HNotate.NoteListDatatypes


snoc g t = t |*> Evt g

-- Abc
upbow' :: Glyph
upbow' = Annotation $ (\d -> char 'u' <> d)

upbow :: EventList -> EventList
upbow = snoc upbow'


downbow' :: Glyph
downbow' = Annotation $ (\d -> char 'v' <> d)

downbow :: EventList -> EventList
downbow = snoc downbow'


-- LilyPond
fermata' :: Glyph 
fermata' = Annotation $ (<> text "\\fermata")

fermata :: EventList -> EventList
fermata = snoc fermata'




