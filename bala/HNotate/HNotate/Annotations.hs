{-# OPTIONS -Wall #-}

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

abcOnly :: (ODoc -> ODoc) -> Annotation
abcOnly fn = Annotation { _ly_anno = id, _abc_anno = fn } 

lyOnly :: (ODoc -> ODoc) -> Annotation
lyOnly fn = Annotation { _ly_anno = fn, _abc_anno = id } 


suffix :: ODoc -> (ODoc -> ODoc)
suffix d = (<> d)

prefix :: ODoc -> (ODoc -> ODoc)
prefix d = (d <>)

--------------------------------------------------------------------------------
--

staccato :: Annotation
staccato = Annotation { _ly_anno = suffix $ string "-.",
                        _abc_anno = prefix $ char '.' }
                        
                        
-- Abc
upbow :: Annotation
upbow = abcOnly (prefix $ char 'u')

downbow :: Annotation
downbow = abcOnly (prefix $ char 'v')



-- LilyPond
fermata :: Annotation 
fermata = lyOnly (suffix $ command "fermata")


stringNum :: Int -> Annotation
stringNum i = lyOnly(suffix $ command (show i))


