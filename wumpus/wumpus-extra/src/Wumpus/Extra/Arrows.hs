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

import Wumpus.Core

import Wumpus.Extra.Arrowheads
import Wumpus.Extra.Base
import Wumpus.Extra.Marks


import Data.AffineSpace
import Data.VectorSpace





-- Notes in TikZ - arrowheads must be able to 'shorten lines' 
-- (vis open triangle arrow heads - the line must be retracted).
-- Lines probably have to be something more /abstract/ than 
-- wumpus-core\'s paths, then they can be /marked/ or decorated
-- vis TikZ.



arrowTri' :: (Floating u, Real u, Mark t) 
          => t -> Point2 u -> Point2 u -> Picture u
arrowTri' attr p1 p2 = frameMulti [thead, arrln]
  where
    lwidth = 1.0
    ang    = langle p1 p2
    thead  = filledStraightTriangle attr ang p2
    arrln  = ostroke () $ path p1 [lineTo $ p2 .-^ avec ang lwidth]
  

arrowHook' :: (Floating u, Real u, Ord u, InnerSpace (Vec2 u), Mark t) 
           => t -> Point2 u -> Point2 u -> Picture u
arrowHook' = strokedArrow doubleHook

arrowPerp' :: (Floating u, Real u, Mark t) 
           => t -> Point2 u -> Point2 u -> Picture u
arrowPerp' = strokedArrow straightPerp


arrowBracket' :: (Floating u, Real u, Mark t) 
              => t -> Point2 u -> Point2 u -> Picture u
arrowBracket' = strokedArrow bracketTip


arrowOutBracket' :: (Floating u, Real u, Mark t) 
              => t -> Point2 u -> Point2 u -> Picture u
arrowOutBracket' = strokedArrow outwardBracketTip

arrowStrokedCurved' :: (Floating u, Real u, InnerSpace (Vec2 u), Mark t) 
                    => t -> Point2 u -> Point2 u -> Picture u
arrowStrokedCurved' = strokedArrow strokedCurvedTip

-- dashes to go at some point...
strokedArrow :: (Floating u, Real u, Mark t)
             => (t -> Radian -> Point2 u -> Primitive u) 
             -> t
             -> Point2 u -> Point2 u
             -> Picture u
strokedArrow fn attr p1 p2 = frameMulti [connector, arrhead]
  where
    ang       = langle p1 p2
    connector = ostroke (DashPattern $ evenDashes 1) $ path p1 [lineTo p2]
    arrhead   = fn attr ang p2 
     
--------------------------------------------------------------------------------
