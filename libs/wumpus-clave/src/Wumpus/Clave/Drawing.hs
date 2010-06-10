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
    drawGraphic
  , wrapG  

  -- * Glyphs
  , circleF
  , barF

  -- * Graphic primitives
  , strokedRectangle
  , filledRectangle
  , strokedCircle
  , filledCircle

  ) where


import Wumpus.Clave.Core
import Wumpus.Clave.Utils

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                 -- package: vector-space


drawGraphic :: (Floating u, Ord u ) => Graphic u -> Maybe (Picture u)
drawGraphic f = step $ f []
  where
    step [] = Nothing
    step xs = Just $ frameMulti xs 


wrapG :: Primitive u -> Graphic u 
wrapG = wrapH 


circleF :: BoxHeight -> DRGB -> DPoint2 -> DGraphic
circleF h rgb = filledCircle rgb radius  
  where
    radius = 0.5 * capHeight h


barF :: BoxHeight -> DRGB -> DPoint2 -> DGraphic
barF h rgb = filledRectangle rgb width height 
  where
    height = capHeight h
    width  = 0.25 * height



--------------------------------------------------------------------------------


strokedRectangle :: (Num u, Ord u, Stroke t) 
                 => t -> u -> u -> Point2 u -> Graphic u
strokedRectangle t w h bl = wrapG $ cstroke t $ rectangle w h bl

filledRectangle :: (Num u, Ord u, Fill t) 
                => t -> u -> u -> Point2 u -> Graphic u
filledRectangle t w h bl = wrapG $ fill t $ rectangle w h bl


strokedCircle :: Fractional u 
              =>  DRGB -> LineWidth -> u -> Point2 u -> Graphic u
strokedCircle rgb lw radius = \pt -> 
    wrapG $ ellipse (rgb, LineWidth lw) radius radius pt

filledCircle :: Fractional u 
             => DRGB -> u -> Point2 u -> Graphic u
filledCircle rgb radius = \pt -> wrapG $ ellipse rgb radius radius pt 


rectangle :: Num u => u -> u -> Point2 u -> Path u
rectangle w h bl = path bl [ lineTo br, lineTo tr, lineTo tl ]
  where
    br = bl .+^ hvec w
    tr = br .+^ vvec h
    tl = bl .+^ vvec h 