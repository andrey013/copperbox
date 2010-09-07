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
-- Graphic types.
--
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic.Primitive
  (
    HPrim
  , Graphic 
  , Image

  , CFGraphic  
  , CFImage

  ) where

import Wumpus.Basic.Graphic.DrawingContext
import Wumpus.Basic.Utils.HList

import Wumpus.Core                      -- package: wumpus-core


-- | Graphics objects, even simple ones (line, arrow, dot) might 
-- need more than one primitive (path or text label) for their
-- construction. Hence, the primary representation that all the 
-- others are built upon must support /concatenation/ of 
-- primitives. 
--
-- Wumpus-Core has a type Picture - made from one or more 
-- Primitives - but Pictures include support for affine frames. 
-- For drawing many simple graphics (dots, connector lines...) 
-- that do not need individual affine transformations this is a 
-- penalty. A list of Primitives is therefore more suitable 
-- representation, and a Hughes list which supports
-- efficient concatenation is wise.
--
type HPrim u = H (Primitive u)


-- | Specifying the drawing properties of a graphic (line width, 
-- font size) is tedious when many properties are shared. Passing
-- a DrawingContext (c.f. the reader monad) to drawing functions
-- is good.
-- 
type Graphic u = DrawingContext -> HPrim u



-- | Image is a /productive/ variant of Graphic. Whereas Graphic  
-- simply produces graphical output apropos the drawing context, 
-- Image produces an /answer/ paired with the output.
--  
-- An example image is a node in a network picture - the node is
-- drawn but it is also produces an /object/ that supports 
-- taking /anchors/ so connectors can be drawn between points on
-- the nodes border.
--
type Image a u = DrawingContext -> (a, HPrim u)


-- | Some graphics take a start point as well as a drawing 
-- context. As the composite pieces of the graphic are drawn 
-- with respect to the supplied start point, they are built in a
-- (somewhat) /coordinate free/ style - so here is a CF 
-- (coordinate-free) Graphic.
--
type CFGraphic u = Point2 u -> Graphic u

-- | A /coordinate free/ Image.
--
type CFImage a u = Point2 u -> Image a u

