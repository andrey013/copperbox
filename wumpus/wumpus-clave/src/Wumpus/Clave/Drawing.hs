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
  

  ) where



import Wumpus.Basic.Kernel              -- package: wumpus-basic

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Core.Colour ( black )

-- locs are center...

circleNh :: FromPtSize u => LocGraphic u
circleNh = markHeight >>= filledDisk


-- can use marks...

triNh    :: (Fractional u, FromPtSize u) => LocGraphic u
triNh    = markHeight >>= \uw -> 
           promote1 $ \ctr -> filledPath $ isoscelesTrianglePath uw uw ctr


{-

circleF :: BoxHeight -> RGBi -> DGraphicF
circleF h rgb = disk rgb radius . disp (radius+dd) (radius+dd)
  where
    radius = fromPtSize $ 0.5 * numeralHeight h
    dd     = fromPtSize $ descenderDepth h


barF :: BoxHeight -> RGBi -> DPoint2 -> DGraphic
barF h rgb = filledRectangle rgb width height  . disp xdisp dd
  where
    height = fromPtSize $ numeralHeight h
    width  = 0.25 * height
    dd     = fromPtSize $ descenderDepth h
    xdisp  = (0.5 * fromIntegral h) - 0.5*width


gridF :: Int -> BoxHeight -> LineWidth -> DPoint2 -> DGraphic
gridF n h lw = border props rect `cc` verts
  where 
    props = (black, LineWidth lw)
    rect  = Rectangle (fromIntegral $ n*h) (fromIntegral h)
    verts = grid props (fromIntegral h) (fromIntegral h) rect


backgroundF :: Int -> BoxHeight -> RGBi -> DPoint2 -> DGraphic
backgroundF n h rgb = wrapG . fill rgb . rectanglePath width height
  where
    height    = fromPtSize $ textHeight h
    width     = height * fromIntegral n

-}