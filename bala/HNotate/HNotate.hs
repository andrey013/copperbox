--------------------------------------------------------------------------------
-- |
-- Module      :  HNotateAlt
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- HNotate - Haskell music notation. 
--
--------------------------------------------------------------------------------

module HNotate (
    module HNotate.BackendAbc,
    module HNotate.BackendLilyPond,
    module HNotate.BackendMidi,    
    module HNotate.Duration,
    module HNotate.EventInterface,
    module HNotate.Pitch,
    module HNotate.ToScore,
    
    module HNotate.OutputLilyPond,
    
    -- restrict the interface to EventTree
      -- * Datatypes
    System, EventTree,
    system, systemL, system1,
    
    root, event,
    
    par, prefix, poly, 
    
    repeated,
    
    ( # ) , ( #. ),
    
    parseLyTemplateExpr, parseLySourceChunks,
    onNoteList, onNoteListM
    
    
 ) where

import HNotate.BackendAbc
import HNotate.BackendLilyPond
import HNotate.BackendMidi hiding (measure_length)
import HNotate.Duration
import HNotate.EventInterface
import HNotate.EventTree
import HNotate.ExtractionDatatypes
import HNotate.ParseAbc (parseAbcTemplateExpr)
import HNotate.ParseLy (parseLyTemplateExpr, parseLySourceChunks)
import HNotate.Pitch
import HNotate.ScoreRepresentation
import HNotate.ToScore

import HNotate.OutputLilyPond

infixl 7 #

( # ) :: a -> (a -> b) -> b
x # f = f x


infixl 7 #.

( #. ) :: (a -> b) -> (b -> c) -> a -> c
g #. f = f . g

