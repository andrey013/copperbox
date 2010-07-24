{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.PSC.Bivariate
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Handling bivariate data and its projection into a drawing 
-- rectangle.
--
--------------------------------------------------------------------------------

module Wumpus.PSC.Bivariate
  (
    Bivariate(..)
  , bivariate
  , xRange
  , yRange
  , borderRectangle
  , borderOrigin
  , borderWidth
  , borderHeight
  , withinBorderRect
  , withinRangeBi
  , scaleX
  , scaleY
  , scaleXY

  ) where

import Wumpus.PSC.Core

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Basic.Graphic                     -- package: wumpus-basic

data Bivariate ux uy = Bivariate 
      { bvp_xrange     :: Range ux
      , bvp_xproj      :: ux -> Double
      , bvp_yrange     :: Range uy
      , bvp_yproj      :: uy -> Double
      , bvp_rect       :: DRectangleLoc
      }



bivariate :: (Range ux, ux -> Double) 
          -> (Range uy, uy -> Double)
          -> DRectangleLoc
          -> Bivariate ux uy
bivariate (xr,fx) (yr,fy) rect@(Rectangle w h, P2 x y)  =  Bivariate 
    { bvp_xrange     = xr
    , bvp_xproj      = xproj
    , bvp_yrange     = yr
    , bvp_yproj      = yproj
    , bvp_rect       = rect
    }
  where
   xproj = projection xr (x ::: (x+w)) fx
   yproj = projection yr (y ::: (y+h)) fy

  
xRange :: Bivariate ux uy -> Range ux
xRange = bvp_xrange

yRange :: Bivariate ux uy -> Range uy
yRange = bvp_yrange

borderRectangle :: Bivariate ux uy -> DRectangleLoc
borderRectangle = bvp_rect

borderOrigin :: Bivariate ux uy -> DPoint2
borderOrigin = snd . borderRectangle

borderWidth :: Bivariate ux uy -> Double
borderWidth = rect_width . fst . borderRectangle

borderHeight :: Bivariate ux uy -> Double
borderHeight = rect_height . fst . borderRectangle


withinBorderRect :: DPoint2 -> Bivariate ux uy -> Bool
withinBorderRect pt = withinRectangleLoc pt . borderRectangle

withinRangeBi :: (Ord ux, Ord uy) => (ux,uy) -> Bivariate ux uy -> Bool
withinRangeBi (ux,uy) bv = 
    withinRange ux (bvp_xrange bv) && withinRange uy (bvp_yrange bv)

-- | Might not fit in the drawing rectangle...
--
scaleX :: ux -> Bivariate ux uy -> Double
scaleX ux bv = (bvp_xproj bv) ux

-- | Might not fit in the drawing rectangle...
--
scaleY :: uy -> Bivariate ux uy -> Double
scaleY uy bv = (bvp_yproj bv) uy


scaleXY :: (ux,uy) -> Bivariate ux uy -> DPoint2
scaleXY (ux,uy) bv = P2 (scaleX ux bv) (scaleY uy bv)


