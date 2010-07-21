{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Graphic
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Graphic type and opertations
--
-- \*\* WARNING \*\* - this module is highly experimental, and 
-- may change significantly or even be dropped from future 
-- revisions.
--
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic
  (
  -- * Type aliases
    Graphic  
  , DGraphic

  , GraphicF
  , DGraphicF

  -- * General combinators
  , cc
  , supply

  -- * Operations
  , drawGraphic
  , wrapG

  , textline
  , straightLine
  , strokedRectangle
  , filledRectangle
  , rectanglePath
  , strokedCircle
  , filledCircle
  , disk

  -- * Displacement
  , Point2T
  , positionWith
  , disp
  , vdisp
  , hdisp

  -- * Grid
  , Rectangle(..)
  , DRectangle
  , grid
  , border

  ) where

import Wumpus.Basic.Graphic.PointSupply
import Wumpus.Basic.Utils.HList

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                 -- package: vector-space

-- | Note - this representation allows for zero, one or more
-- Primitives to be collected together.
--
type Graphic u          = H (Primitive u)

type DGraphic           = Graphic Double

type GraphicF u         = Point2 u -> Graphic u

type DGraphicF          = GraphicF Double


--------------------------------------------------------------------------------
-- Combinators...

infixr 9 `cc`

-- | Composition operator...
--
-- > cc f g = \x y -> f x (g x y)
--
cc :: (r1 -> a -> ans) -> (r1 -> r2 -> a) -> r1 -> r2 -> ans
cc f g = \x y -> f x (g x y)


-- | Reverse application.
--
supply :: u -> (u -> a) -> a 
supply u f = f u


--------------------------------------------------------------------------------

-- | Note - a Picture cannot be empty whereas a Graphic can.
-- Hence this function returns via Maybe.
--
drawGraphic :: (Real u, Floating u) => Graphic u -> Maybe (Picture u)
drawGraphic f = post $ f []
  where
    post [] = Nothing
    post xs = Just $ frameMulti $ xs 


-- | Lift a Primitive to a Graphic
--
wrapG :: Primitive u -> Graphic u
wrapG = wrapH 


--------------------------------------------------------------------------------

-- | Text should not contain newlines.
--
-- Note the supplied point is the \'left-baseline\'.
--
textline :: (TextLabel t, Num u) => t -> String -> GraphicF u
textline t ss = wrapG . textlabel t ss 


-- | Vector is applied to the point.
--
straightLine :: (Stroke t, Fractional u) => t -> Vec2 u -> GraphicF u
straightLine t v = \pt -> wrapG $ ostroke t $ path pt [lineTo $ pt .+^ v]
 

-- | Supplied point is center.
--
strokedRectangle :: (Stroke t, Fractional u) => t -> u -> u -> GraphicF u
strokedRectangle t w h = wrapG . cstroke t . rectangle w h

-- | Supplied point is center.
--
filledRectangle :: (Fill t, Fractional u) => t -> u -> u -> GraphicF u
filledRectangle t w h = wrapG . fill t . rectangle w h

rectangle :: Fractional u => u -> u -> Point2 u -> Path u
rectangle w h ctr = rectanglePath w h (ctr .-^ vec (0.5*w) (0.5*h))

-- | Supplied point is /bottom-left/.
--
rectanglePath :: Num u => u -> u -> Point2 u -> Path u
rectanglePath w h bl = path bl [ lineTo br, lineTo tr, lineTo tl ]
  where
    br = bl .+^ hvec w
    tr = br .+^ vvec h
    tl = bl .+^ vvec h 


-- | 'strokedCircle' : @ stroked_props * num_subs * radius -> GraphicF @
--
-- Draw a stroked circle made from Bezier curves. @num_subs@ is 
-- the number of subdivisions per quadrant.
--
-- The result is a HOF (GraphicF :: Point -> Graphic) where the 
-- point is the center. 
-- 
strokedCircle :: (Stroke t, Floating u) => t -> Int -> u -> GraphicF u
strokedCircle t n r = wrapG . cstroke t . curvedPath . bezierCircle n r


-- | 'filledCircle' : @ fill_props * num_subs * radius -> GraphicF @
--
-- Draw a filled circle made from Bezier curves. @num_subs@ is 
-- the number of subdivisions per quadrant.
--
-- The result is a HOF (GraphicF :: Point -> Graphic) where the 
-- point is the center. 
--
filledCircle :: (Fill t, Floating u) => t -> Int -> u -> GraphicF u
filledCircle t n r = wrapG . fill t . curvedPath . bezierCircle n r



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
disk :: (Ellipse t, Fractional u) => t -> u -> GraphicF u
disk t radius = wrapG . ellipse t radius radius 


--------------------------------------------------------------------------------
-- Transforming points...


type Point2T    u = Point2 u -> Point2 u

positionWith :: Point2T u -> (Point2 u -> a) -> (Point2 u -> a)
positionWith displacer gf  = gf . displacer 


disp :: Num u => u -> u -> Point2T u
disp x y = (.+^ V2 x y)

hdisp :: Num u => u -> Point2T u
hdisp x = disp x 0

vdisp :: Num u => u -> Point2T u
vdisp y = disp 0 y

--------------------------------------------------------------------------------
-- need a border / frame abstraction...

data Rectangle u = Rectangle 
      { rect_width     :: !u
      , rect_height    :: !u 
      }  
  deriving (Eq,Ord,Show)

type DRectangle = Rectangle Double

-- | 'grid' : @ stroke_props * xstep * ystep * boundary_rect -> GraphicF @
--
-- The result is a HOF (GraphicF :: Point -> Graphic) where the 
-- point is bottom-left. 
--
grid :: (Stroke t, RealFrac u) => t -> u -> u -> Rectangle u -> GraphicF u 
grid t xstep ystep (Rectangle w h) = \pt ->
    vlines pt . hlines pt
  where
    vlines (P2 x y) = veloH (straightLine t (vvec h)) $ hpoints y xstep (x,x+w)
    hlines (P2 x y) = veloH (straightLine t (hvec w)) $ vpoints x ystep (y,y+h)
    

-- | 'border' : @ stroke_props * boundary_rect -> GraphicF @
--
-- The result is a HOF (GraphicF :: Point -> Graphic) where the 
-- point is bottom-left. 
--
border :: (Stroke t, Num u) => t -> Rectangle u -> GraphicF u
border t (Rectangle w h) = wrapG . cstroke t . rectanglePath w h