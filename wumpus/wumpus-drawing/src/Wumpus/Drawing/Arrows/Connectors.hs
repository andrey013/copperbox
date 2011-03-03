{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Arrows.Connectors
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Draw arrows.
--
--------------------------------------------------------------------------------

module Wumpus.Drawing.Arrows.Connectors
  ( 

    PathConnector
  , DPathConnector

  , leftArrow
  , rightArrow
  , leftRightArrow
  , uniformArrow

  ) where

import Wumpus.Drawing.Arrows.Tips
import Wumpus.Drawing.Paths

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

-- An arrowhead always know how to draw itself (filled triangle, 
-- stroked barb, etc.)
--
-- A Path is currently always drawn with openStroke, eventually 
-- there might be scope for drawing e.g. parallel lines  ==== or 
-- decorations.
--


-- | A connector with optional arrow tips. The connector is an 
-- /Image/, drawing it returns the path - positions can be taken 
-- on the path (e.g. @midpoint@) for further decoration.
--
type PathConnector u    = ConnectorImage Path u
type DPathConnector     = PathConnector Double

-- Design note - the above definitions should be in a different 
-- file. 




-- | Connector with an arrow tip at the start point \/ left.
--
leftArrow :: (Real u, Floating u, PtSize u) 
           => Arrowhead u -> PathCF u -> PathConnector u
leftArrow arrh conn = promoteR2 $ \p0 p1 -> 
    connect conn p0 p1           >>= \cpath -> 
    arrowhead_retract_dist arrh  >>= \dl -> 
    let path1   = shortenL dl cpath
        ang     = directionL path1
        start   = tipL cpath
        g1      = openStroke $ toPrimPath path1
        g2      = atRot (arrowhead_draw arrh) start ang
    in  replaceAns cpath $ g1 `oplus` g2       

-- Note - returns original path and adds tips to the @cpath@ 
-- which might have moved p0 and p1.
                 

-- | Connector with an arrow tip at the end point \/ right.
--
rightArrow :: (Real u, Floating u, PtSize u) 
           => Arrowhead u -> PathCF u -> PathConnector u
rightArrow arrh conn = promoteR2 $ \p0 p1 -> 
    connect conn p0 p1           >>= \cpath -> 
    arrowhead_retract_dist arrh  >>= \dr -> 
    let path1   = shortenR dr cpath
        ang     = directionR path1
        end     = tipR cpath
        g1      = openStroke $ toPrimPath path1
        g2      = atRot (arrowhead_draw arrh) end ang
    in  replaceAns cpath $ g1 `oplus` g2



-- | Connector with two arrow tips, possibly different.
--
leftRightArrow :: (Real u, Floating u, PtSize u) 
               => Arrowhead u -> Arrowhead u -> PathCF u 
               -> PathConnector u
leftRightArrow arrL arrR conn = promoteR2 $ \p0 p1 -> 
    connect conn p0 p1           >>= \cpath -> 
    arrowhead_retract_dist arrL  >>= \dL -> 
    arrowhead_retract_dist arrR  >>= \dR -> 
    let path1   = shortenPath dL dR cpath
        angL    = directionL path1
        angR    = directionR path1
        start   = tipL cpath
        end     = tipR cpath
        g1      = openStroke $ toPrimPath path1
        gL      = atRot (arrowhead_draw arrL) start angL
        gR      = atRot (arrowhead_draw arrR) end   angR
    in  replaceAns cpath $ g1 `oplus` gL `oplus` gR


-- | Connector with the same arrow tip at both ends.
--
uniformArrow :: (Real u, Floating u, PtSize u) 
             => Arrowhead u -> PathCF u -> PathConnector u
uniformArrow arrh cp = leftRightArrow arrh arrh cp

