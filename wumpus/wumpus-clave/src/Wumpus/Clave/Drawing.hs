{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Clave.Drawing
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Core types, functions...
--
--------------------------------------------------------------------------------

module Wumpus.Clave.Drawing
  (
  
  -- * Graphic
    wrapG  

  -- * Glyphs
  , circleF
  , barF
  , gridF
  , backgroundF

  -- * Graphic primitives
  , strokedCircle
  , filledCircle

  ) where


import Wumpus.Clave.Core

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Core.Colour ( black )
import Wumpus.Basic.Graphic             -- package: wumpus-basic
import Wumpus.Basic.Utils.HList

import Data.AffineSpace                 -- package: vector-space


displacePt :: Num u => u -> u -> (Point2 u -> Point2 u)
displacePt x y = (.+^ V2 x y)

circleF :: BoxHeight -> DRGB -> DGraphicF
circleF h rgb = disk rgb radius . displacePt (radius+dd) (radius+dd)
  where
    radius = 0.5 * numeralHeight h
    dd     = descenderDepth h


barF :: BoxHeight -> DRGB -> DPoint2 -> DGraphic
barF h rgb = filledRectangle rgb width height  . displacePt xdisp dd
  where
    height = numeralHeight h
    width  = 0.25 * height
    dd     = descenderDepth h
    xdisp  = (0.5 * fromIntegral h) - 0.5*width

gridF :: Int -> BoxHeight -> LineWidth -> DPoint2 -> DGraphic
gridF n h lw = \ pt -> rect pt . verts pt
  where
    height    = textHeight h
    width     = height * fromIntegral n
    rect pt   = wrapG $ cstroke (black,LineWidth lw) $ rectanglePath width height pt
    verts pt  = veloH (\i ->  mkLine (pt .+^ hvec (height * fromIntegral i)))
                      [1..n-1]
    mkLine    = straightLine (black, LineWidth lw) (vvec height)

backgroundF :: Int -> BoxHeight -> DRGB -> DPoint2 -> DGraphic
backgroundF n h rgb = wrapG . fill rgb . rectanglePath width height
  where
    height    = textHeight h
    width     = height * fromIntegral n


--------------------------------------------------------------------------------



