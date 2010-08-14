{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Arrows
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Anchor points on shapes.
--
-- \*\* WARNING \*\* this module is an experiment, and may 
-- change significantly or even be dropped from future revisions.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Arrows
  ( 
    line
  , arrowTri90
  , arrowTri60
  , arrowTri45
  , arrowOTri90
  , arrowOTri60
  , arrowOTri45
  
  , arrowPerp

  ) where

import Wumpus.Basic.Arrows.Tips
import Wumpus.Basic.Graphic
import Wumpus.Basic.Graphic.DrawingAttr
import Wumpus.Basic.Monads.Drawing

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                 -- package: vector-space

-- connectors need to be something rather than () ...

line :: Num u => AGraphic2 u ()
line = AGraphic2 id lineGraphic mf
  where
    mf _    _  _  = ()


lineGraphic :: Num u => DrawingAttr -> Point2 u -> Point2 u -> Graphic u
lineGraphic attr p1 p2 = 
    wrapG $ ostroke (strokeAttr attr) $ vertexPath [p1,p2]


retractByCharWidth :: (Real u, Floating u, FromPtSize u) 
                   => DrawingAttr -> Point2 u -> Point2 u -> Point2 u
retractByCharWidth attr p1 p2 = p2 .-^ v
  where
    v0 = p2 .-. p1
    sz = fromPtSize $ xcharHeight $ font_size $ font_props attr
    v  = avec (direction v0) sz

arrTriGraphic :: (Real u, Floating u, FromPtSize u) 
              => (DrawingAttr -> Radian -> GraphicF u)
              -> DrawingAttr -> Point2 u -> Point2 u -> Graphic u
arrTriGraphic tipF attr p1 p2 = lineGraphic attr p1 end . tipF attr theta p2 
  where
    end   = retractByCharWidth attr p1 p2
    theta = langle p1 p2 


arrowTri90 :: (Real u, Floating u, FromPtSize u) => AGraphic2 u ()
arrowTri90 = AGraphic2 id (arrTriGraphic tri90) mf
  where
    mf _    _  _  = ()

arrowTri60 :: (Real u, Floating u, FromPtSize u) => AGraphic2 u ()
arrowTri60 = AGraphic2 id (arrTriGraphic tri60) mf
  where
    mf _    _  _  = ()

arrowTri45 :: (Real u, Floating u, FromPtSize u) => AGraphic2 u ()
arrowTri45 = AGraphic2 id (arrTriGraphic tri45) mf
  where
    mf _    _  _  = ()

arrowOTri90 :: (Real u, Floating u, FromPtSize u) => AGraphic2 u ()
arrowOTri90 = AGraphic2 id (arrTriGraphic otri90) mf
  where
    mf _    _  _  = ()

arrowOTri60 :: (Real u, Floating u, FromPtSize u) => AGraphic2 u ()
arrowOTri60 = AGraphic2 id (arrTriGraphic otri60) mf
  where
    mf _    _  _  = ()

arrowOTri45 :: (Real u, Floating u, FromPtSize u) => AGraphic2 u ()
arrowOTri45 = AGraphic2 id (arrTriGraphic otri45) mf
  where
    mf _    _  _  = ()

arrowPerp :: (Real u, Floating u, FromPtSize u) => AGraphic2 u ()
arrowPerp = AGraphic2 id df mf
  where
    df attr p1 p2 = let theta = langle p1 p2 in
                    lineGraphic attr p1 p2 . perp attr theta p2
    mf _    _  _  = ()
