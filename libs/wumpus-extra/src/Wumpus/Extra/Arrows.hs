{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Arrows
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Arrows...
--
--------------------------------------------------------------------------------

module Wumpus.Extra.Arrows where

import Wumpus.Extra.Arrowheads
-- import Wumpus.Geometry
import Wumpus.Geometry.CoreAdditions

import Wumpus.Core

import Data.Aviary

import Data.AffineSpace
import Data.VectorSpace




-- Still don't know what to do about line width...
type LineW u = u

-- Notes in TikZ - arrowheads must be able to 'shorten lines' 
-- (vis open triangle arrow heads - the line must be retracted).
-- Lines probably have to be something more /abstract/ than 
-- wumpus-core\'s paths, then they can be /marked/ or decorated
-- vis TikZ.


arrowTri' :: (Floating u, Real u) => Point2 u -> Point2 u -> Picture u
arrowTri' p1 p2 = frameMulti [thead, arrln]
  where
    lwidth = 1.0
    ang    = langle p1 p2
    thead  = filledStraightTriangle () lwidth ang p2
    arrln  = ostroke () $ path p1 [lineTo $ p2 .-^ avec ang lwidth]
  

arrowHook' :: (Floating u, Real u, Ord u, InnerSpace (Vec2 u)) 
           => Point2 u -> Point2 u -> Picture u
arrowHook' = strokedArrow () doubleHook

arrowPerp' :: (Floating u, Real u) 
           => Point2 u -> Point2 u -> Picture u
arrowPerp' = strokedArrow () straightPerp


-- dashes to go at some point...
strokedArrow :: (Floating u, Real u, Stroke t)
             => t 
             -> (t -> u -> Radian -> Point2 u -> Primitive u) 
             -> Point2 u -> Point2 u
             -> Picture u
strokedArrow attr fn p1 p2 = frameMulti [connector, arrhead]
  where
    lwidth    = 1.0
    ang       = langle p1 p2
    connector = ostroke (DashPattern $ evenDashes 1) $ path p1 [lineTo p2]
    arrhead   = fn attr lwidth ang p2 
     
--------------------------------------------------------------------------------
