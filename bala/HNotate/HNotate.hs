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
    module HNotate.DebugUtils,
    module HNotate.Duration,
    module HNotate.Pitch,   
    module HNotate.OutputMain,
    
    -- restrict the interface to EventTree
      -- * Datatypes
    System, EventList, Evt,
    system, systemL, system1,
    
    root, note, rest, spacer,    
    chord, gracenotes, poly, 
    notelist,
    
    {- repeated, eventlist, -}
    
    ( # ) , ( #. )    
    
 ) where

import HNotate.CommonUtils
import HNotate.DebugUtils
import HNotate.Duration
import HNotate.NoteListDatatypes
import HNotate.Pitch
import HNotate.OutputMain



