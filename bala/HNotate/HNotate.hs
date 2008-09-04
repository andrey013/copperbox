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
    module HNotate.EventInterface,
    module HNotate.Pitch,   
    module HNotate.OutputMain,
    
    -- restrict the interface to EventTree
      -- * Datatypes
    System, EventList,
    system, systemL, system1,
    
    root, event,
    
    par, prefix, poly, 
    
    repeated, eventlist,
    
    ( # ) , ( #. )    
    
 ) where

import HNotate.DebugUtils
import HNotate.Duration
import HNotate.EventInterface
import HNotate.EventList
import HNotate.Pitch
import HNotate.OutputMain

infixl 7 #

( # ) :: a -> (a -> b) -> b
x # f = f x


infixl 7 #.

( #. ) :: (a -> b) -> (b -> c) -> a -> c
g #. f = f . g

