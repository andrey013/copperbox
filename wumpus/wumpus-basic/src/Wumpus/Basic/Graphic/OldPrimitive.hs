{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Graphic.OldPrimitive
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Graphic types and operations.
--
-- \*\* WARNING \*\* - this due a major revision and will change
-- significantly (or disappear...).
--
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic.OldPrimitive
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
  , drawGraphicU

  , wrapG
  , emptyG 

  -- * Graphic primitives
  , textline
  , xtextline
  , straightLine
  , strokedRectangle
  , filledRectangle
  , rectanglePath
  , strokedCircle
  , filledCircle
  , disk

  -- * Displacement
  , Point2T
  , DPoint2T 
  , positionWith
  , disp
  , vdisp
  , hdisp

  -- * Grid
  , Rectangle(..)
  , DRectangle
  , grid
  , border

  , RectangleLoc
  , DRectangleLoc
  , withinRectangleLoc

  ) where

import Wumpus.Basic.Graphic.PointSupply
import Wumpus.Basic.Utils.HList

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                 -- package: vector-space

import Data.Maybe



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
drawGraphic :: (Real u, Floating u, FromPtSize u) 
            => Graphic u -> Maybe (Picture u)
drawGraphic f = post $ f []
  where
    post [] = Nothing
    post xs = Just $ frame xs 


-- | /Unsafe/ version of 'drawGraphic' - this function throws 
-- an error when the graphic is empty.
--
drawGraphicU :: (Real u, Floating u, FromPtSize u) => Graphic u -> Picture u
drawGraphicU = fromMaybe errK . drawGraphic
  where
    errK = error "drawGraphic - empty Graphic."


-- | Lift a Primitive to a Graphic
--
wrapG :: Primitive u -> Graphic u
wrapG = wrapH 

-- | The empty graphic.
--
emptyG :: Graphic u
emptyG = emptyH

--------------------------------------------------------------------------------

-- | Text should not contain newlines.
--
-- Note the supplied point is the \'left-baseline\'.
--
textline :: Num u => RGBi -> FontAttr -> String -> GraphicF u
textline rgb attr ss = wrapG . textlabel rgb attr ss 

xtextline :: Num u => RGBi -> FontAttr -> XLink -> String -> GraphicF u
xtextline rgb attr xl ss = wrapG . xtextlabel rgb attr xl ss 



-- | Vector is applied to the point.
--
straightLine :: Fractional u => RGBi -> StrokeAttr -> Vec2 u -> GraphicF u
straightLine rgb attr v = 
    \pt -> wrapG $ ostroke rgb attr $ path pt [lineTo $ pt .+^ v]
 

-- | Supplied point is center.
--
strokedRectangle :: Fractional u => RGBi -> StrokeAttr -> u -> u -> GraphicF u
strokedRectangle rgb attr w h = wrapG . cstroke rgb attr . rectangle w h

-- | Supplied point is center.
--
filledRectangle :: Fractional u => RGBi -> u -> u -> GraphicF u
filledRectangle rgb w h = wrapG . fill rgb . rectangle w h

rectangle :: Fractional u => u -> u -> Point2 u -> PrimPath u
rectangle w h ctr = rectanglePath w h (ctr .-^ vec (0.5*w) (0.5*h))

-- | Supplied point is /bottom-left/.
--
rectanglePath :: Num u => u -> u -> Point2 u -> PrimPath u
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
strokedCircle :: Floating u => RGBi -> StrokeAttr -> Int -> u -> GraphicF u
strokedCircle rgb attr n r = 
    wrapG . cstroke rgb attr . curvedPath . bezierCircle n r


-- | 'filledCircle' : @ fill_props * num_subs * radius -> GraphicF @
--
-- Draw a filled circle made from Bezier curves. @num_subs@ is 
-- the number of subdivisions per quadrant.
--
-- The result is a HOF (GraphicF :: Point -> Graphic) where the 
-- point is the center. 
--
filledCircle :: Floating u => RGBi -> Int -> u -> GraphicF u
filledCircle rgb n r = wrapG . fill rgb . curvedPath . bezierCircle n r



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
disk :: Fractional u => RGBi -> u -> GraphicF u
disk rgb radius = wrapG . fillEllipse rgb radius radius 


--------------------------------------------------------------------------------
-- Transforming points...


type Point2T    u = Point2 u -> Point2 u

type DPoint2T     = Point2T Double

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
grid :: RealFrac u => RGBi -> StrokeAttr -> u -> u -> Rectangle u -> GraphicF u 
grid rgb attr xstep ystep (Rectangle w h) = \pt ->
    vlines pt . hlines pt
  where
    vlines (P2 x y) = veloH (straightLine rgb attr (vvec h)) $ hpoints y xstep (x,x+w)
    hlines (P2 x y) = veloH (straightLine rgb attr (hvec w)) $ vpoints x ystep (y,y+h)
    

-- | 'border' : @ stroke_props * boundary_rect -> GraphicF @
--
-- The result is a HOF (GraphicF :: Point -> Graphic) where the 
-- point is bottom-left. 
--
border :: Num u => RGBi -> StrokeAttr -> Rectangle u -> GraphicF u
border rgb attr (Rectangle w h) = wrapG . cstroke rgb attr . rectanglePath w h



type RectangleLoc u = (Rectangle u, Point2 u)

type DRectangleLoc = RectangleLoc Double


withinRectangleLoc :: (Num u, Ord u) => Point2 u -> RectangleLoc u -> Bool
withinRectangleLoc (P2 x y) (Rectangle w h, P2 ox oy) = 
   ox <= x && x <= (ox+w) && oy <= y && y <= (oy+h)



