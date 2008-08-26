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
    module BackendAbc,
    module BackendLilyPond,
    module BackendMidi,    
    module Duration,
    module EventInterface,
    module Pitch,
    module ToScore,
    
    -- restrict the interface to EventTree
      -- * Datatypes
    System, EventTree,
    system, systemL, system1,
    
    root, event,
    
    par, prefix, poly, 
    
    repeated,
    
    ( # ) , ( #. )  
    
    
 ) where

import BackendAbc
import BackendLilyPond
import BackendMidi hiding (measure_length)
import Duration
import EventInterface
import EventTree
import Pitch
import ScoreRepresentation
import ToScore



infixl 7 #

( # ) :: a -> (a -> b) -> b
x # f = f x


infixl 7 #.

( #. ) :: (a -> b) -> (b -> c) -> a -> c
g #. f = f . g

