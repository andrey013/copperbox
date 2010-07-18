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
-- Drawing...
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



circleF :: BoxHeight -> DRGB -> DGraphicF
circleF h rgb = disk rgb radius . disp (radius+dd) (radius+dd)
  where
    radius = 0.5 * numeralHeight h
    dd     = descenderDepth h


barF :: BoxHeight -> DRGB -> DPoint2 -> DGraphic
barF h rgb = filledRectangle rgb width height  . disp xdisp dd
  where
    height = numeralHeight h
    width  = 0.25 * height
    dd     = descenderDepth h
    xdisp  = (0.5 * fromIntegral h) - 0.5*width


gridF :: Int -> BoxHeight -> LineWidth -> DPoint2 -> DGraphic
gridF n h lw = border props rect `cc` verts
  where 
    props = (black, LineWidth lw)
    rect  = RectFrame (fromIntegral $ n*h) (fromIntegral h)
    verts = grid props (fromIntegral h) (fromIntegral h) rect


backgroundF :: Int -> BoxHeight -> DRGB -> DPoint2 -> DGraphic
backgroundF n h rgb = wrapG . fill rgb . rectanglePath width height
  where
    height    = textHeight h
    width     = height * fromIntegral n

