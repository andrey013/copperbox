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
-- ** WARNING ** this module is highly experimental, and may 
-- change significantly or even be dropped from future revisions.
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

  -- * New Bird..
  , cc

  -- * Operations
  , drawGraphic
  , wrapG

  , textline
  , straightLine
  , strokedRectangle
  , filledRectangle
  , strokedCircle
  , filledCircle
  , disk

  ) where



import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Basic.Utils.HList

import Data.AffineSpace                 -- package: vector-space

-- | Note - this representation allows for zero, one or more
-- Primitives to be collected together.
--
type Graphic u = H (Primitive u)

type DGraphic  = Graphic Double

type GraphicF u = Point2 u -> Graphic u

type DGraphicF = GraphicF Double


--------------------------------------------------------------------------------
-- Wow a new bird combinator...

infixr 9 `cc`

cc :: (r1 -> a -> ans) -> (r1 -> r2 -> a) -> r1 -> r2 -> ans
cc f g = \x y -> f x (g x y)


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
textline :: (TextLabel t, Num u) => t -> String -> GraphicF u
textline t ss = wrapG . textlabel t ss 

straightLine :: (Stroke t, Num u) => t -> Vec2 u -> GraphicF u
straightLine t v = \pt -> wrapG $ ostroke t $ path pt [lineTo $ pt .+^ v]


-- | Point is bottom-left.
--
strokedRectangle :: (Stroke t, Num u) => t -> u -> u -> GraphicF u
strokedRectangle t w h = wrapG . cstroke t . rectangle w h

-- | Point is bottom-left.
--
filledRectangle :: (Fill t, Num u) => t -> u -> u -> GraphicF u
filledRectangle t w h = wrapG . fill t . rectangle w h


rectangle :: Num u => u -> u -> Point2 u -> Path u
rectangle w h bl = path bl [ lineTo br, lineTo tr, lineTo tl ]
  where
    br = bl .+^ hvec w
    tr = br .+^ vvec h
    tl = bl .+^ vvec h 


-- | Point is center, n is number of subdivisions per quadrant.
--
-- Circle is made from bezier curves.
-- 
strokedCircle :: (Stroke t, Floating u) => t -> Int -> u -> GraphicF u
strokedCircle t n r = wrapG . cstroke t . curvedPath . bezierCircle n r

-- | Point is center, n is number of subdivisions per quadrant.
--
-- Circle is made from bezier curves.
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


