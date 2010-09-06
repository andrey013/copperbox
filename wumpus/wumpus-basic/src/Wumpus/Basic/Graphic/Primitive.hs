{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Graphic.Primitive
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Graphic types and operations.
--
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic.Primitive
  (
    HPrim
  , Graphic 
  , CFGraphic  

  ) where

import Wumpus.Basic.Graphic.DrawingAttr
import Wumpus.Basic.Utils.HList

import Wumpus.Core                      -- package: wumpus-core


-- | Graphics objects, even simple ones (line, arrow, dot) might 
-- need more than one primitive (path or text label) for their
-- construction. Hence, the primal representation above 
-- Wumpus-Core must support /concatenation/. Wumpus-Core does
-- support Pictures - made of one or Primitives - but they have
-- penalty of supporting affine frames. A list of Primitives is 
-- therefore more suitable, and a Hughes list which supports
-- efficient concatenation is wise.
--
type HPrim u = H (Primitive u)

-- TODO - DrawingAttr needs a name change, here it gets a synonym.
--
type DrawingContext = DrawingAttr

-- | Specifying the drawing properties of a graphic (line width, 
-- font size) is tedious when many properties are shared. Passing
-- a DrawingContext (c.f. the reader monad) to drawing functions
-- is good.
-- 
type Graphic u = DrawingContext -> HPrim u


-- | Some graphics take a start point as well as a drawing 
-- context. As the composite pieces of the graphic are drawn 
-- with respect to the supplied start point, they are built in a
-- (somewhat) /coordinate free/ style - so here is a CF 
-- (coordinate-free) Graphic.
--
type CFGraphic u = Point2 u -> Graphic u