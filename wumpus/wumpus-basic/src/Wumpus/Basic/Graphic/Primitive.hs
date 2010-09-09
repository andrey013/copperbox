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
    HPrimitive
  , Graphic 
  , Image
  , RImage(..)

  , asks
  , localCtx

  , runImage

  , CFGraphic  
  , CFImage

  ) where

import Wumpus.Basic.Graphic.DrawingContext
import Wumpus.Basic.Utils.HList

import Wumpus.Core                      -- package: wumpus-core

import Control.Applicative

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
type HPrimitive u = H (Primitive u)


-- If this is extended to a two params we can /hide/ xlink
-- within the type and not need hyperlink versions of all 
-- functions 
--
-- img = DrawingContext -> XLink -> a
--
-- Or maybe a link version of /fontDeltaContext/ is needed...
-- ... the latter is the one to follow...
-- 

newtype RImage a = RImage { getRImage :: DrawingContext -> a }

type Image u = RImage (HPrimitive u)



instance Functor RImage where
  fmap f ma = RImage $ \ctx -> f $ getRImage ma ctx 


-- Applicative

instance Applicative RImage where
  pure a    = RImage $ \_   -> a
  mf <*> ma = RImage $ \ctx -> let f = getRImage mf ctx
                                   a = getRImage ma ctx
                               in f a

-- Monad 

instance Monad RImage where
  return a  = RImage $ \_   -> a
  ma >>= k  = RImage $ \ctx -> let a = getRImage ma ctx
                               in (getRImage . k) a ctx 

asks :: (DrawingContext -> a) -> RImage a
asks fn = RImage $ \ctx -> fn ctx

localCtx :: (DrawingContext -> DrawingContext) -> RImage a -> RImage a
localCtx upd img = RImage $ \ctx -> getRImage img (upd ctx)

runImage :: DrawingContext -> Image u -> HPrimitive u
runImage ctx img = (getRImage img) ctx


--------------------------------------------------------------------------------


-- | Specifying the drawing properties of a graphic (line width, 
-- font size) is tedious when many properties are shared. Passing
-- a DrawingContext (c.f. the reader monad) to drawing functions
-- is good.
-- 
type Graphic u = DrawingContext -> HPrimitive u



-- | Image is a /productive/ variant of Graphic. Whereas Graphic  
-- simply produces graphical output apropos the drawing context, 
-- Image produces an /answer/ paired with the output.
--  
-- An example image is a node in a network picture - the node is
-- drawn but it is also produces an /object/ that supports 
-- taking /anchors/ so connectors can be drawn between points on
-- the nodes border.
--
-- type Image a u = DrawingContext -> (a, HPrimitive u)


-- | Some graphics take a start point as well as a drawing 
-- context. As the composite pieces of the graphic are drawn 
-- with respect to the supplied start point, they are built in a
-- (somewhat) /coordinate free/ style - so here is a CF 
-- (coordinate-free) Graphic.
--
type CFGraphic u = Point2 u -> Graphic u

-- | A /coordinate free/ Image.
--
type CFImage u = Point2 u -> Image u


