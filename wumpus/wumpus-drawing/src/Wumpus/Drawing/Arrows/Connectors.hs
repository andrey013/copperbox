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

    ArrowConnector
  , leftArrow
  , rightArrow
  , leftRightArrow
  , uniformArrow

  ) where

import Wumpus.Basic.Kernel
import Wumpus.Drawing.Arrows.Tips
import Wumpus.Drawing.Paths

import Wumpus.Core                      -- package: wumpus-core


-- An arrowhead always know how to draw itself (filled triangle, 
-- stroked barb, etc.)
--
-- A Path is currently always drawn with openStroke,
-- eventually there might be scope for drawing 
-- e.g. parallel lines  ====
--



type ArrowConnector u = ConnectorImage u (Path u)


-- One for Wumpus-Basic.ContextFun
--
atRot :: LocThetaCF u a -> Point2 u -> Radian -> CF a
atRot = apply2R2


-- for Paths.Base ?
--
shortenPath :: (Real u , Floating u) => u  -> u -> Path u -> Path u
shortenPath l r = shortenL l .  shortenR r 


leftArrow :: (Real u, Floating u) 
           => Arrowhead u -> ConnectorPath u -> ArrowConnector u
leftArrow arrh conn = promoteR2 $ \p0 p1 -> 
    connect conn p0 p1           >>= \cpath -> 
    arrowhead_retract_dist arrh  >>= \dl -> 
    let path1   = shortenL dl cpath
        ang     = directionL path1
        g1      = openStroke $ toPrimPath path1
        g2      = atRot (arrowhead_draw arrh) p0 ang
    in  fmap (replaceL cpath) $ g1 `oplus` g2       

-- Note - returns original path
                 

rightArrow :: (Real u, Floating u) 
           => Arrowhead u -> ConnectorPath u -> ArrowConnector u
rightArrow arrh conn = promoteR2 $ \p0 p1 -> 
    connect conn p0 p1           >>= \cpath -> 
    arrowhead_retract_dist arrh  >>= \dr -> 
    let path1   = shortenR dr cpath
        ang     = directionR path1
        g1      = openStroke $ toPrimPath path1
        g2      = atRot (arrowhead_draw arrh) p1 ang
    in  fmap (replaceL cpath) $ g1 `oplus` g2



leftRightArrow :: (Real u, Floating u) 
               => Arrowhead u -> Arrowhead u -> ConnectorPath u 
               -> ArrowConnector u
leftRightArrow arrL arrR conn = promoteR2 $ \p0 p1 -> 
    connect conn p0 p1           >>= \cpath -> 
    arrowhead_retract_dist arrL  >>= \dL -> 
    arrowhead_retract_dist arrR  >>= \dR -> 
    let path1   = shortenPath dL dR cpath
        angL    = directionL path1
        angR    = directionR path1
        g1      = openStroke $ toPrimPath path1
        gL      = atRot (arrowhead_draw arrL) p0 angL
        gR      = atRot (arrowhead_draw arrR) p1 angR
    in  fmap (replaceL cpath) $ g1 `oplus` gL `oplus` gR


-- | Same tip both ends.
--
uniformArrow :: (Real u, Floating u) 
             => Arrowhead u -> ConnectorPath u -> ArrowConnector u
uniformArrow arrh cp = leftRightArrow arrh arrh cp

