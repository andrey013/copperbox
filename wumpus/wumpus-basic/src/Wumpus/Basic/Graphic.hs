{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Graphic
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
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

  , text
  , straightLine
  , strokedRectangle
  , filledRectangle
  , circle

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

text :: (TextLabel t, Num u) => t -> String -> GraphicF u
text t ss = wrapG . textlabel t ss 

straightLine :: (Stroke t, Num u) => t -> Vec2 u -> GraphicF u
straightLine t v = \pt -> wrapG $ ostroke t $ path pt [lineTo $ pt .+^ v]


-- | Point is bottom-left.
--
strokedRectangle :: (Stroke t, Num u, Ord u) => t -> u -> u -> GraphicF u
strokedRectangle t w h = wrapG . cstroke t . rectangle w h

-- | Point is bottom-left.
--
filledRectangle :: (Fill t, Num u, Ord u) => t -> u -> u -> GraphicF u
filledRectangle t w h = wrapG . fill t . rectangle w h


rectangle :: Num u => u -> u -> Point2 u -> Path u
rectangle w h bl = path bl [ lineTo br, lineTo tr, lineTo tl ]
  where
    br = bl .+^ hvec w
    tr = br .+^ vvec h
    tl = bl .+^ vvec h 



circle :: (Ellipse t, Fractional u) => t -> u -> GraphicF u
circle t radius = wrapG . ellipse t radius radius 
