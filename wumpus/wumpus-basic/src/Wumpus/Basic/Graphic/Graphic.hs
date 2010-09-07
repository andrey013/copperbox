{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Graphic.Graphic
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Elementary functions for the Graphic type.
--
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic.Graphic
  (
    drawGraphic
  , drawGraphicU

  , supplyPt
  , localCF

  , textline
  , xtextline

  , straightLine
  , xstraightLine

  , strokedRectangle
  , xstrokedRectangle

  , filledRectangle
  , xfilledRectangle

  , borderedRectangle
  , xborderedRectangle


  , strokedCircle
  , xstrokedCircle

  , filledCircle
  , xfilledCircle

  , borderedCircle
  , xborderedCircle
  
  , disk
  , xdisk

  ) where

import Wumpus.Basic.Graphic.DrawingContext
import Wumpus.Basic.Graphic.Primitive
import Wumpus.Basic.Utils.HList

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

-- | Draw a graphic with an initial drawing context.
-- 
-- Note - a Picture cannot be empty whereas a Graphic can.
-- Hence this function returns via Maybe.
--
drawGraphic :: (Real u, Floating u, FromPtSize u) 
            => DrawingContext -> Graphic u -> Maybe (Picture u)
drawGraphic ctx gf = post $ gf ctx []
  where
    post [] = Nothing
    post xs = Just $ frame xs


-- | /Unsafe/ version of 'drawGraphic' - this function throws 
-- an error when the graphic is empty.
--
drawGraphicU :: (Real u, Floating u, FromPtSize u) 
             => DrawingContext -> Graphic u -> Picture u
drawGraphicU ctx gf = post $ gf ctx []
  where
    post [] = error "drawGraphicU - empty graphic."
    post xs = frame xs


--------------------------------------------------------------------------------


-- | The C' combinator (cardinal prime).
--
-- For CFGraphic, this models looking into the DrawingCtx but
-- using the initial point as is.
--
combCF :: (a -> r1 -> ans) -> (r2 -> a) -> r1 -> r2 -> ans
combCF f pf r1 r2 = f (pf r2) r1

-- Version of combCF with two functions to project from the 
-- DrawingCtx
--
combCF2 :: (a -> b -> r1 -> ans) -> (r2 -> a) -> (r2 -> b)
        -> r1 -> r2 -> ans
combCF2 f pf1 pf2 r1 r2 = f (pf1 r2) (pf2 r2) r1

-- Version of combCF with three functions to project from the 
-- DrawingCtx
--
combCF3 :: (a -> b -> c -> r1 -> ans) -> (r2 -> a) -> (r2 -> b) -> (r2 -> c)
        -> r1 -> r2 -> ans
combCF3 f pf1 pf2 pf3 r1 r2 = f (pf1 r2) (pf2 r2) (pf3 r2) r1


-- | Supplying a point to a 'CFGraphic' takes it to a regular 
-- 'Graphic'.
--
supplyPt :: Point2 u -> CFGraphic u -> Graphic u
supplyPt pt gf = gf pt 

--------------------------------------------------------------------------------

localCF :: (DrawingContext -> DrawingContext) -> CFGraphic u -> CFGraphic u
localCF upd gf = \pt attr -> gf pt (upd attr) 


-- | Text should not contain newlines.
--
-- Note the supplied point is the \'left-baseline\'.
--
textline :: Num u => String -> CFGraphic u
textline ss = \pt ctx -> let (rgb,attr) = textAttr ctx 
                         in wrapH $ textlabel rgb attr ss pt

-- | Hyperlink version of 'textline'.
--
xtextline :: Num u => XLink -> String -> CFGraphic u
xtextline xlink ss = 
    combCF (\(rgb,attr) -> wrapH . xtextlabel rgb attr xlink ss)
           textAttr


-- | Vector is applied to the point.
--
straightLine :: Fractional u => Vec2 u -> CFGraphic u
straightLine v = 
    combCF2 (\rgb attr pt -> let pp = path pt [lineTo $ pt .+^ v] in
                             wrapH $ ostroke rgb attr pp)
            primary_colour 
            stroke_props 


-- | Vector is applied to the point.
--
xstraightLine :: Fractional u => XLink -> Vec2 u -> CFGraphic u
xstraightLine xlink v = 
    combCF2 (\rgb attr pt -> let pp = path pt [lineTo $ pt .+^ v] in
                             wrapH $ xostroke rgb attr xlink pp)
            primary_colour 
            stroke_props 


--------------------------------------------------------------------------------
-- Rectangles

rectangleCtr :: Fractional u => u -> u -> Point2 u -> PrimPath u
rectangleCtr w h ctr = rectangleBL w h (ctr .-^ vec (0.5*w) (0.5*h))

-- | Supplied point is /bottom-left/.
--
rectangleBL :: Num u => u -> u -> Point2 u -> PrimPath u
rectangleBL w h bl = path bl [ lineTo br, lineTo tr, lineTo tl ]
  where
    br = bl .+^ hvec w
    tr = br .+^ vvec h
    tl = bl .+^ vvec h 



-- | Supplied point is center.
--
strokedRectangle :: Fractional u => u -> u -> CFGraphic u
strokedRectangle = xstrokedRectangle NoLink

-- | Supplied point is center.
--
xstrokedRectangle :: Fractional u => XLink -> u -> u -> CFGraphic u
xstrokedRectangle xlink w h = 
    combCF2 (\rgb attr pt -> 
                wrapH $ xcstroke rgb attr xlink $ rectangleCtr w h pt)
            primary_colour
            stroke_props


-- | Supplied point is center.
--
filledRectangle :: Fractional u => u -> u -> CFGraphic u
filledRectangle = xfilledRectangle NoLink


-- | Supplied point is center.
--
xfilledRectangle :: Fractional u => XLink -> u -> u -> CFGraphic u
xfilledRectangle xlink w h = 
    combCF (\rgb pt -> wrapH $ xfill rgb xlink $ rectangleCtr w h pt)
           secondary_colour
  

-- | Supplied point is center.
--
borderedRectangle :: Fractional u => u -> u -> CFGraphic u
borderedRectangle = xborderedRectangle NoLink

-- | Supplied point is center.
--
xborderedRectangle :: Fractional u => XLink -> u -> u -> CFGraphic u
xborderedRectangle xlink w h = 
    combCF3 (\frgb sa srgb pt -> 
                wrapH $ xbordered srgb sa frgb xlink $ rectangleCtr w h pt)
            primary_colour
            stroke_props
            secondary_colour

--------------------------------------------------------------------------------

-- | 'strokedCircle' : @ num_subs * radius -> CFGraphic @
--
-- Draw a stroked circle made from Bezier curves. @num_subs@ is 
-- the number of subdivisions per quadrant.
--
-- The result is a HOF (GraphicF :: Point -> Graphic) where the 
-- point is the center. 
-- 
strokedCircle :: Floating u => Int -> u -> CFGraphic u
strokedCircle = xstrokedCircle NoLink


xstrokedCircle :: Floating u => XLink -> Int -> u -> CFGraphic u
xstrokedCircle xlink n r = 
    combCF2 (\rgb attr pt -> wrapH $ xcstroke rgb attr xlink 
                                   $ curvedPath $ bezierCircle n r pt)
            primary_colour
            stroke_props


filledCircle :: Floating u => Int -> u -> CFGraphic u
filledCircle = xfilledCircle NoLink

xfilledCircle :: Floating u => XLink -> Int -> u -> CFGraphic u
xfilledCircle xlink n r = 
    combCF (\rgb pt -> 
                wrapH $ xfill rgb xlink $ curvedPath $ bezierCircle n r pt)
           secondary_colour
      

borderedCircle :: Floating u => Int -> u -> CFGraphic u
borderedCircle = xborderedCircle NoLink

xborderedCircle :: Floating u => XLink -> Int -> u -> CFGraphic u
xborderedCircle xlink n r = 
    combCF3 (\frgb sa srgb pt -> 
                wrapH $ xbordered srgb sa frgb xlink 
                      $ curvedPath $ bezierCircle n r pt)
           primary_colour
           stroke_props
           secondary_colour
      


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
disk ::  Fractional u => u -> CFGraphic u
disk = xdisk NoLink

xdisk :: Fractional u => XLink -> u -> CFGraphic u
xdisk xlink radius = 
    combCF (\rgb pt -> wrapH $ xfillEllipse rgb xlink radius radius pt)
           secondary_colour