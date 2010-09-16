{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Graphic.PrimGraphic
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Elementary functions for the Graphic and LocGraphic types.
--
-- The functions here are generally analogeous to the Picture 
-- API in @Wumpus.Core@, but here they exploit the implicit 
-- @DrawingContext@.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic.PrimGraphic
  (
    drawGraphic
  , drawGraphicU


  , openStroke
  , closedStroke
  , filledPath
  , borderedPath
  , textline
  , strokedEllipse
  , filledEllipse  
  , borderedEllipse

  , supplyPt
  , localDrawingContext
  , localPoint
  , displace

  , straightLine

  , strokedRectangle
  , filledRectangle
  , borderedRectangle


  , strokedCircle
  , filledCircle
  , borderedCircle
  
  , strokedDisk
  , filledDisk
  , borderedDisk
  

  ) where

import Wumpus.Basic.Graphic.DrawingContext
import Wumpus.Basic.Graphic.BaseTypes

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Control.Applicative

drawGraphic :: (Real u, Floating u, FromPtSize u) 
            => DrawingContext -> Graphic u -> Maybe (Picture u)
drawGraphic ctx gf = post $ runGraphic ctx gf
  where
    post hf = let xs = hprimToList hf in 
              if null xs then Nothing else Just (frame xs)

drawGraphicU :: (Real u, Floating u, FromPtSize u) 
             => DrawingContext -> Graphic u -> Picture u
drawGraphicU ctx gf = post $ runGraphic ctx gf
  where
    post hf = let xs = hprimToList hf in 
              if null xs then errK else frame xs
    errK    = error "drawGraphicU - empty Graphic."




-- having the same names is actually not so useful...

openStroke :: Num u => PrimPath u -> Graphic u
openStroke pp = (\rgb attr -> singleH $ ostroke rgb attr pp) 
                    <$> asksDF primary_colour <*> asksDF stroke_props


closedStroke :: Num u => PrimPath u -> Graphic u
closedStroke pp = (\rgb attr -> singleH $ cstroke rgb attr pp) 
                      <$> asksDF primary_colour <*> asksDF stroke_props


filledPath :: Num u => PrimPath u -> Graphic u
filledPath pp = (\rgb -> singleH $ fill rgb pp) 
                    <$> asksDF secondary_colour


borderedPath :: Num u => PrimPath u -> Graphic u
borderedPath pp = 
    (\frgb attr srgb -> singleH $ fillStroke frgb attr srgb pp) 
        <$> asksDF secondary_colour <*> asksDF stroke_props <*> asksDF primary_colour


textline :: Num u => String -> LocGraphic u
textline ss baseline_left =
    (\(rgb,attr) -> singleH $ textlabel rgb attr ss baseline_left) 
       <$> asksDF textAttr


strokedEllipse :: Num u => u -> u -> LocGraphic u
strokedEllipse hw hh pt =  
    (\rgb attr -> singleH $ strokeEllipse rgb attr hw hh pt) 
       <$> asksDF primary_colour <*> asksDF stroke_props

filledEllipse :: Num u => u -> u -> LocGraphic u
filledEllipse hw hh pt =  
    (\rgb -> singleH $ fillEllipse rgb hw hh pt) 
       <$> asksDF secondary_colour

borderedEllipse :: Num u => u -> u -> LocGraphic u
borderedEllipse hw hh pt = 
    (\frgb attr srgb -> singleH $ fillStrokeEllipse frgb attr srgb hw hh pt) 
        <$> asksDF secondary_colour <*> asksDF stroke_props 
        <*> asksDF primary_colour


--------------------------------------------------------------------------------


-- | Supplying a point to a 'CFGraphic' takes it to a regular 
-- 'Graphic'.
--
supplyPt :: Point2 u -> LocGraphic u -> Graphic u
supplyPt pt gf = gf pt 


displace :: Num u => u -> u -> Point2 u -> Point2 u
displace dx dy (P2 x y) = P2 (x+dx) (y+dy)


localDrawingContext :: 
    (DrawingContext -> DrawingContext) -> LocGraphic u -> LocGraphic u
localDrawingContext upd img = \pt -> localDF upd (img pt) 

localPoint :: (Point2 u -> Point2 u) -> LocGraphic u -> LocGraphic u
localPoint upd gf = \pt -> gf (upd pt)


--------------------------------------------------------------------------------


straightLine :: Fractional u => Vec2 u -> LocGraphic u
straightLine v = \pt -> openStroke $ path pt [lineTo $ pt .+^ v]
           

-- | Supplied point is /bottom-left/.
--
rectangle :: Num u => u -> u -> Point2 u -> PrimPath u
rectangle w h bl = path bl [ lineTo br, lineTo tr, lineTo tl ]
  where
    br = bl .+^ hvec w
    tr = br .+^ vvec h
    tl = bl .+^ vvec h 



-- | Supplied point is /bottom left/.
--
strokedRectangle :: Fractional u => u -> u -> LocGraphic u
strokedRectangle w h = closedStroke . rectangle w h



-- | Supplied point is /bottom left/.
--
filledRectangle :: Fractional u => u -> u -> LocGraphic u
filledRectangle w h = filledPath . rectangle w h
  

-- | Supplied point is /bottom left/.
--
borderedRectangle :: Fractional u => u -> u -> LocGraphic u
borderedRectangle w h = borderedPath . rectangle w h

--------------------------------------------------------------------------------


-- | Supplied point is center. Circle is drawn with Bezier 
-- curves. 
--
strokedCircle :: Floating u => Int -> u -> LocGraphic u
strokedCircle n r = closedStroke . curvedPath . bezierCircle n r



-- | Supplied point is center. Circle is drawn with Bezier 
-- curves. 
--
filledCircle :: Floating u => Int -> u -> LocGraphic u
filledCircle n r = filledPath . curvedPath . bezierCircle n r


-- | Supplied point is center. Circle is drawn with Bezier 
-- curves. 
--
borderedCircle :: Floating u => Int -> u -> LocGraphic u
borderedCircle n r = borderedPath . curvedPath . bezierCircle n r


-- | 'disk' is drawn with Wumpus-Core\'s @ellipse@ primitive.
--
-- This is a efficient representation of circles using 
-- PostScript\'s @arc@ or SVG\'s @circle@ in the generated 
-- output. However, stroked-circles do not draw well after 
-- non-uniform scaling - the line width is scaled as well as 
-- the shape.
--
-- For stroked circles that can be scaled, consider making the 
-- circle from Bezier curves.
--
strokedDisk :: Num u => u -> LocGraphic u
strokedDisk radius = strokedEllipse radius radius


filledDisk :: Num u => u -> LocGraphic u
filledDisk radius = filledEllipse radius radius

borderedDisk :: Num u => u -> LocGraphic u
borderedDisk radius = borderedEllipse radius radius
