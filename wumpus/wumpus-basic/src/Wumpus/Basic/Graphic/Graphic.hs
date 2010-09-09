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
    drawImage
  , drawImageU



  , supplyPt
  , localDrawingContext
  , localPoint

  , RectOrientation(..)
  , textline

  , straightLine

  , strokedRectangle
  , filledRectangle
  , borderedRectangle


  , strokedCircle
  , filledCircle
  , borderedCircle
  
  , disk

  ) where

import Wumpus.Basic.Graphic.DrawingContext
import Wumpus.Basic.Graphic.Primitive
import Wumpus.Basic.Utils.HList

import qualified Wumpus.Core            as WC   -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Control.Applicative

drawImage :: (Real u, Floating u, WC.FromPtSize u) 
          => DrawingContext -> Image u -> Maybe (WC.Picture u)
drawImage ctx img = post $ runImage ctx img
  where
    post hf = let xs = toListH hf in 
              if null xs then Nothing else Just (WC.frame xs)

drawImageU :: (Real u, Floating u, WC.FromPtSize u) 
          => DrawingContext -> Image u -> WC.Picture u
drawImageU ctx img = post $ runImage ctx img
  where
    post hf = let xs = toListH hf in 
              if null xs then errK else WC.frame xs
    errK    = error "drawImageU - empty Image."



-- BIG NOTICE - there is some value in redoing the Core.Picture 
-- interface with extraction from DrawingContext, it should clear
-- up a lot of this code...


ostroke :: Num u => WC.PrimPath u -> Image u
ostroke path = (\rgb attr -> wrapH $ WC.ostroke rgb attr path) 
                  <$> asks primary_colour <*> asks stroke_props


cstroke :: Num u => WC.PrimPath u -> Image u
cstroke path = (\rgb attr -> wrapH $ WC.cstroke rgb attr path) 
                  <$> asks primary_colour <*> asks stroke_props


fill :: Num u => WC.PrimPath u -> Image u
fill path = (\rgb -> wrapH $ WC.fill rgb path) 
              <$> asks secondary_colour


bordered :: Num u => WC.PrimPath u -> Image u
bordered path = (\frgb attr srgb -> wrapH $ WC.bordered frgb attr srgb path) 
                  <$> asks secondary_colour <*> asks stroke_props 
                  <*> asks primary_colour


textlabel :: Num u => String -> CFImage u
textlabel ss baseline_left =
    (\(rgb,attr) -> wrapH $ WC.textlabel rgb attr ss baseline_left) 
       <$> asks textAttr


strokeEllipse :: Num u => u -> u -> CFImage u
strokeEllipse hw hh pt =  
    (\rgb attr -> wrapH $ WC.strokeEllipse rgb attr hw hh pt) 
       <$> asks primary_colour <*> asks stroke_props

fillEllipse :: Num u => u -> u -> CFImage u
fillEllipse hw hh pt =  
    (\rgb -> wrapH $ WC.fillEllipse rgb hw hh pt) 
       <$> asks secondary_colour

borderedEllipse :: Num u => u -> u -> CFImage u
borderedEllipse hw hh pt = 
    (\frgb attr srgb -> wrapH $ WC.borderedEllipse frgb attr srgb hw hh pt) 
        <$> asks secondary_colour <*> asks stroke_props 
        <*> asks primary_colour


--------------------------------------------------------------------------------


-- | Supplying a point to a 'CFGraphic' takes it to a regular 
-- 'Graphic'.
--
supplyPt :: WC.Point2 u -> CFImage u -> Image u
supplyPt pt gf = gf pt 



--------------------------------------------------------------------------------

displace :: Num u => u -> u -> WC.Point2 u -> WC.Point2 u
displace dx dy (WC.P2 x y) = WC.P2 (x+dx) (y+dy)


localDrawingContext :: 
    (DrawingContext -> DrawingContext) -> CFImage u -> CFImage u
localDrawingContext upd img = \pt -> localCtx upd (img pt) 

localPoint :: (WC.Point2 u -> WC.Point2 u) -> CFImage u -> CFImage u
localPoint upd gf = \pt -> gf (upd pt)

-- some things need to be monadic...
contextualPoint :: (DrawingContext -> a) 
               -> (a -> WC.Point2 u -> WC.Point2 u) 
               -> CFImage u 
               -> CFImage u
contextualPoint extr upd img pt = asks extr >>= \a -> img (upd a pt)


straightLine :: Fractional u => WC.Vec2 u -> CFImage u
straightLine v = \pt -> ostroke $ WC.path pt [WC.lineTo $ pt .+^ v]
           


-- Note - its reasonable to want text (and rectangles) drawn in 
-- two ways:
--
-- 1 - supplied point is center
-- 2 - supplied point is bottom left (baseline left for text)
--

data RectOrientation = RECT_LLC | RECT_CTR
  deriving (Eq,Ord,Show)




textline :: (Fractional u, WC.FromPtSize u)
           => RectOrientation -> String -> CFImage u
textline RECT_LLC ss = textlabel ss
textline RECT_CTR ss = 
    contextualPoint (textDimensions ss) 
                    (\(w,h) -> displace (0.5 * (-w)) (0.5 * (-h)))
                    (textlabel ss)



--------------------------------------------------------------------------------
-- Rectangles

rectangleCtr :: Fractional u => u -> u -> WC.Point2 u -> WC.PrimPath u
rectangleCtr w h ctr = rectangleBL w h (ctr .-^ WC.vec (0.5*w) (0.5*h))

-- | Supplied point is /bottom-left/.
--
rectangleBL :: Num u => u -> u -> WC.Point2 u -> WC.PrimPath u
rectangleBL w h bl = WC.path bl [ WC.lineTo br, WC.lineTo tr, WC.lineTo tl ]
  where
    br = bl .+^ WC.hvec w
    tr = br .+^ WC.vvec h
    tl = bl .+^ WC.vvec h 



-- | Supplied point is center.
--
strokedRectangle :: Fractional u => u -> u -> CFImage u
strokedRectangle w h pt = 
    (\rgb attr -> wrapH $ WC.cstroke rgb attr $ rectangleCtr w h pt)
      <$> asks primary_colour <*> asks stroke_props



-- | Supplied point is center.
--
filledRectangle :: Fractional u => u -> u -> CFImage u
filledRectangle w h pt = 
    (\rgb -> wrapH $ WC.fill rgb $ rectangleCtr w h pt)
       <$> asks secondary_colour
  

-- | Supplied point is center.
--
borderedRectangle :: Fractional u => u -> u -> CFImage u
borderedRectangle w h pt = 
    (\frgb sa srgb -> wrapH $ WC.bordered srgb sa frgb $ rectangleCtr w h pt)
      <$> asks primary_colour <*> asks stroke_props <*> asks secondary_colour

--------------------------------------------------------------------------------


strokedCircle :: Floating u => Int -> u -> CFImage u
strokedCircle n r pt = 
    (\rgb attr -> wrapH $ WC.cstroke rgb attr 
                        $ WC.curvedPath $ WC.bezierCircle n r pt)
      <$> asks primary_colour <*> asks stroke_props



filledCircle :: Floating u => Int -> u -> CFImage u
filledCircle n r pt = 
    (\rgb -> wrapH $ WC.fill rgb $ WC.curvedPath $ WC.bezierCircle n r pt)
        <$> asks secondary_colour
      


borderedCircle :: Floating u => Int -> u -> CFImage u
borderedCircle n r pt = 
    (\frgb sa srgb -> wrapH $ WC.bordered srgb sa frgb
                            $ WC.curvedPath $ WC.bezierCircle n r pt)
      <$> asks primary_colour <*> asks stroke_props
                              <*> asks secondary_colour
      


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
disk ::  Fractional u => u -> CFImage u
disk radius = fillEllipse radius radius
