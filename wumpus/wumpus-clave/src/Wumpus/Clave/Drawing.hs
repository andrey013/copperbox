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
  , gridF
  , backgroundF

  -- * Graphic primitives
  , straightLine
  , strokedRectangle
  , filledRectangle
  , strokedCircle
  , filledCircle

  ) where


import Wumpus.Clave.Core

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Core.Colour ( black )
import Wumpus.Basic.Utils.HList         -- package: wumpus-basic

import Data.AffineSpace                 -- package: vector-space

drawGraphic :: (Floating u, Ord u ) => Graphic u -> Maybe (Picture u)
drawGraphic f = step $ f []
  where
    step [] = Nothing
    step xs = Just $ frameMulti $ xs 


wrapG :: Primitive u -> Graphic u 
wrapG = wrapH 

displacePt :: Num u => u -> u -> (Point2 u -> Point2 u)
displacePt x y = (.+^ V2 x y)

circleF :: BoxHeight -> DRGB -> DPoint2 -> DGraphic
circleF h rgb = filledCircle rgb radius . displacePt (radius+dd) (radius+dd)
  where
    radius = 0.5 * capHeight h
    dd     = descenderDepth h


barF :: BoxHeight -> DRGB -> DPoint2 -> DGraphic
barF h rgb = filledRectangle rgb width height  . displacePt xdisp dd
  where
    height = capHeight h
    width  = 0.25 * height
    dd     = descenderDepth h
    xdisp  = (0.5 * fromIntegral h) - 0.5*width

gridF :: Int -> BoxHeight -> LineWidth -> DPoint2 -> DGraphic
gridF n h lw = \ pt -> rect pt . verts pt
  where
    height    = textHeight h
    width     = height * fromIntegral n
    rect pt   = strokedRectangle (black,LineWidth lw) width height pt
    verts pt  = veloH (\i ->  mkLine (pt .+^ hvec (height * fromIntegral i)))
                      [1..n-1]
    mkLine    = straightLine (black, LineWidth lw) (vvec height)

backgroundF :: Int -> BoxHeight -> DRGB -> DPoint2 -> DGraphic
backgroundF n h rgb = filledRectangle rgb width height
  where
    height    = textHeight h
    width     = height * fromIntegral n


--------------------------------------------------------------------------------


straightLine :: (Num u, Ord u, Stroke t) 
             => t -> Vec2 u -> Point2 u -> Graphic u
straightLine t v = \pt -> wrapG $ ostroke t $ path pt [lineTo $ pt .+^ v]

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