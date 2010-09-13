{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Graphic.Image
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Image types.
--
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic.Image
  (
    HPrim
  , Point2T
  , DrawingObject(..)
  , LocDrawingObject

  , Graphic
  , appendGraphic
  
  , asksObj
  , localCtxObj

  , runGraphic

  , LocGraphic
  , Image
  , LocImage
  , runImage
  , intoImage
  , intoLocImage

  , ConnDrawingObject
  , ConnGraphic
  , ConnImage

  , intoConnImage

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
type HPrim u = H (PrimElement u)


-- | Point transformation function.
--
type Point2T u = Point2 u -> Point2 u


newtype DrawingObject a = DrawingObject { 
          getDrawingObject :: DrawingContext -> a }

type LocDrawingObject u a = Point2 u -> DrawingObject a 



instance Functor DrawingObject where
  fmap f ma = DrawingObject $ \ctx -> f $ getDrawingObject ma ctx 


-- Applicative

instance Applicative DrawingObject where
  pure a    = DrawingObject $ \_   -> a
  mf <*> ma = DrawingObject $ \ctx -> let f = getDrawingObject mf ctx
                                          a = getDrawingObject ma ctx
                                      in f a

-- Monad 

instance Monad DrawingObject where
  return a  = DrawingObject $ \_   -> a
  ma >>= k  = DrawingObject $ \ctx -> let a = getDrawingObject ma ctx
                                      in (getDrawingObject . k) a ctx 

-- Simple drawing - representing one or more prims

type Graphic u = DrawingObject (HPrim u)

appendGraphic :: Graphic u -> Graphic u -> Graphic u
appendGraphic gf1 gf2 = DrawingObject $ \ctx ->          
      (getDrawingObject gf1 ctx) `appendH` (getDrawingObject gf2 ctx)



asksObj :: (DrawingContext -> a) -> DrawingObject a
asksObj fn = DrawingObject $ \ctx -> fn ctx

localCtxObj :: (DrawingContext -> DrawingContext) 
            -> DrawingObject a -> DrawingObject a
localCtxObj upd gf = DrawingObject $ \ctx -> getDrawingObject gf (upd ctx)

runGraphic :: DrawingContext -> Graphic u -> HPrim u
runGraphic ctx gf = (getDrawingObject gf) ctx


--------------------------------------------------------------------------------


-- | Commonly graphics take a start point as well as a drawing 
-- context.
-- 
-- Here they are called a LocGraphic - graphic with a (starting) 
-- location.
--
type LocGraphic u = Point2 u -> Graphic u


type Image u a = DrawingObject (a, HPrim u)

type LocImage u a = Point2 u -> Image u a


runImage :: DrawingContext -> Image u a -> (a,HPrim u)
runImage ctx img = (getDrawingObject img) ctx


intoImage :: DrawingObject a -> Graphic u -> Image u a
intoImage f g = DrawingObject $ \ctx -> 
    let a = getDrawingObject f ctx; o = getDrawingObject g ctx in (a,o)


intoLocImage :: LocDrawingObject u a -> LocGraphic u -> LocImage u a
intoLocImage f g pt = DrawingObject $ \ctx -> 
    let a = getDrawingObject (f pt) ctx; 
        o = getDrawingObject (g pt) ctx 
    in (a,o)

type ConnDrawingObject u a = Point2 u -> Point2 u -> DrawingObject a

-- | ConnGraphic is a connector drawn between two points 
-- contructing a Graphic.
--
type ConnGraphic u = Point2 u -> Point2 u -> Graphic u

-- | ConImage is a connector drawn between two points 
-- constructing an Image.
--
type ConnImage u a = Point2 u -> Point2 u -> Image u a


intoConnImage :: ConnDrawingObject u a -> ConnGraphic u -> ConnImage u a
intoConnImage f g p1 p2 = DrawingObject $ \ctx -> 
    let a = getDrawingObject (f p1 p2) ctx; 
        o = getDrawingObject (g p1 p2) ctx 
    in (a,o)
