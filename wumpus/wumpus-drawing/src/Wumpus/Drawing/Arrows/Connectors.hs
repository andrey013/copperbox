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
  , PathQuery
  , DPathQuery

  , leftArrow
  , rightArrow
  , leftRightArrow
  , uniformArrow

  ) where

import Wumpus.Drawing.Arrows.Tips
import Wumpus.Drawing.Paths

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

-- import Wumpus.Core                              -- package: wumpus-core

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
type PathConnector u    = ConnectorImage u (AbsPath u)
type DPathConnector     = PathConnector Double


-- | Note - a PathCF is a context function not a graphic 
--  

type PathQuery u = ConnectorQuery u (AbsPath u)

type DPathQuery  = PathQuery Double

-- Design note - the above definitions should be in a different 
-- file. 




-- | Connector with an arrow tip at the start point \/ left.
--
leftArrow :: (Real u, Floating u, InterpretUnit u) 
           => Arrowhead u -> PathQuery u -> PathConnector u
leftArrow arrh conn = promoteR2 $ \p0 p1 -> 
    apply2R2 conn p0 p1            >>= \cpath -> 
    arrowhead_retract_dist arrh    >>= \dl -> 
    let path1   = shortenL dl cpath
        ang     = directionL path1
        start   = tipL cpath
        g2      = atIncline (arrowhead_draw arrh) start ang
    in fmap (replaceAns cpath) $ decorateR0 g2 $ toPrimPath path1 >>= openStroke

-- Note - returns original path and adds tips to the @cpath@ 
-- which might have moved p0 and p1.
                 

-- | Connector with an arrow tip at the end point \/ right.
--
rightArrow :: (Real u, Floating u, InterpretUnit u) 
           => Arrowhead u -> PathQuery u -> PathConnector u
rightArrow arrh conn = promoteR2 $ \p0 p1 -> 
    apply2R2 conn p0 p1          >>= \cpath -> 
    arrowhead_retract_dist arrh  >>= \dr -> 
    let path1   = shortenR dr cpath
        ang     = directionR path1
        end     = tipR cpath
        g2      = atIncline (arrowhead_draw arrh) end ang
    in fmap (replaceAns cpath) $ decorateR0 g2 $ toPrimPath path1 >>= openStroke



-- | Connector with two arrow tips, possibly different.
--
leftRightArrow :: (Real u, Floating u, InterpretUnit u) 
               => Arrowhead u -> Arrowhead u -> PathQuery u 
               -> PathConnector u
leftRightArrow arrL arrR conn = promoteR2 $ \p0 p1 -> 
    apply2R2 conn p0 p1           >>= \cpath -> 
    arrowhead_retract_dist arrL   >>= \dL -> 
    arrowhead_retract_dist arrR   >>= \dR -> 
    let path1   = shortenPath dL dR cpath
        angL    = directionL path1
        angR    = directionR path1
        start   = tipL cpath
        end     = tipR cpath
        gL      = atIncline (arrowhead_draw arrL) start angL
        gR      = atIncline (arrowhead_draw arrR) end   angR
    in fmap (replaceAns cpath) $ decorateR0 (gL `oplus` gR) $ 
        toPrimPath path1 >>= openStroke


-- | Connector with the same arrow tip at both ends.
--
uniformArrow :: (Real u, Floating u, InterpretUnit u) 
             => Arrowhead u -> PathQuery u -> PathConnector u
uniformArrow arrh cp = leftRightArrow arrh arrh cp

