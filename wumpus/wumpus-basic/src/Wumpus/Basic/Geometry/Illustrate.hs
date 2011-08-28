{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Geometry.Illustrate
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Draw the geometrical objects.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Geometry.Illustrate
  ( 

    illustrateLine
  , illustrateLineSegment

  ) 
  where

import Wumpus.Basic.Geometry.Base

import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Colour 

import Data.Monoid


emline :: InterpretUnit u => Vec2 Em -> LocGraphic u
emline = uconvF . locStraightLine

enDot :: InterpretUnit u => LocGraphic u
enDot = uconvF body
  where
    body :: LocGraphic En
    body = localize (fill_colour white) $ dcDisk DRAW_FILL_STROKE 0.5

illustrateLine :: (Real u, Floating u, InterpretUnit u) 
               => Line u -> Graphic u
illustrateLine (Line p1 p2) = 
    mconcat [ prefix, join_line, suffix, d1, d2 ]
  where
    join_line = straightLine p1 p2
    v1        = pvec p1 p2
    dir       = vdirection v1
    
    prefix    = localize dotted_line $ emline (avec dir (-6)) `at` p1
    suffix    = localize dotted_line $ emline (avec dir 6)    `at` p2
    d1        = enDot `at` p1
    d2        = enDot `at` p2

       



illustrateLineSegment :: InterpretUnit u => LineSegment u -> Graphic u
illustrateLineSegment (LineSegment p1 p2) = 
    join_line `mappend` d1 `mappend` d2
  where
    join_line = straightLine p1 p2
    d1        = enDot `at` p1
    d2        = enDot `at` p2
