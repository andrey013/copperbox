{-# LANGUAGE TypeFamilies               #-}
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
-- Base types for Drawing Objects, Graphics / Images (a Graphic 
-- that also returns an answer), etc.
--
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic.BaseTypes
  (
    

    HPrim
  , Point2T
  , DPoint2T

  , DrawingObject(..)
  , LocDrawingObject
  , DLocDrawingObject

  , liftDrawingObject 

  , Graphic
  , DGraphic
  , appendGraphic
  , gcat
  
  , asksObj
  , localCtxObj

  , runGraphic

  , LocGraphic
  , DLocGraphic
  , appendAt

  , Image
  , DImage
  , LocImage
  , DLocImage

  , runImage
  , intoImage
  , intoLocImage

  , ConnDrawingObject
  , DConnDrawingObject
  , ConnGraphic
  , DConnGraphic
  , ConnImage
  , DConnImage

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

type DPoint2T = Point2T Double


newtype DrawingObject a = DrawingObject { 
          getDrawingObject :: DrawingContext -> a }



type LocDrawingObject u a = Point2 u -> DrawingObject a 

type DLocDrawingObject a = LocDrawingObject Double a



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


liftDrawingObject :: a -> DrawingObject a
liftDrawingObject a = DrawingObject $ \ _ctx -> a 

-- Simple drawing - representing one or more prims

type Graphic u = DrawingObject (HPrim u)

type DGraphic = Graphic Double


appendGraphic :: Graphic u -> Graphic u -> Graphic u
appendGraphic gf1 gf2 = DrawingObject $ \ctx ->          
      (getDrawingObject gf1 ctx) `appendH` (getDrawingObject gf2 ctx)

gcat :: Graphic u -> [Graphic u] -> Graphic u
gcat a = step a 
  where
    step ac []     = ac
    step ac (x:xs) = step (ac `appendGraphic` x) xs
 

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

type DLocGraphic = LocGraphic Double




-- | Composition operator for LocGraphic - both LocGraphics
-- are drawn at the same origin and the results concatenated.
--
--
appendAt :: LocGraphic u -> LocGraphic u -> LocGraphic u
appendAt f g = \pt -> f pt `appendGraphic` g pt


--------------------------------------------------------------------------------


-- | Images return a value as well as drawing. A /node/ is a 
-- typical example - nodes are drawing but the also support 
-- taking anchor points.
--
type Image u a = DrawingObject (a, HPrim u)

type DImage a = Image Double a

type LocImage u a = Point2 u -> Image u a

type DLocImage a = LocImage Double a

runImage :: DrawingContext -> Image u a -> (a,HPrim u)
runImage ctx img = (getDrawingObject img) ctx


intoImage :: DrawingObject a -> Graphic u -> Image u a
intoImage f g = DrawingObject $ \ctx -> 
    let a = getDrawingObject f ctx; o = getDrawingObject g ctx in (a,o)


intoLocImage :: LocDrawingObject u a -> LocGraphic u -> LocImage u a
intoLocImage f g pt = DrawingObject $ \ctx -> 
    let a = getDrawingObject (f pt) ctx
        o = getDrawingObject (g pt) ctx 
    in (a,o)

type ConnDrawingObject u a = Point2 u -> Point2 u -> DrawingObject a

type DConnDrawingObject a = ConnDrawingObject Double a

-- | ConnGraphic is a connector drawn between two points 
-- contructing a Graphic.
--
type ConnGraphic u = Point2 u -> Point2 u -> Graphic u

type DConnGraphic = ConnGraphic Double

-- | ConImage is a connector drawn between two points 
-- constructing an Image.
--
type ConnImage u a = Point2 u -> Point2 u -> Image u a

type DConnImage a = ConnImage Double a


intoConnImage :: ConnDrawingObject u a -> ConnGraphic u -> ConnImage u a
intoConnImage f g p1 p2 = DrawingObject $ \ctx -> 
    let a = getDrawingObject (f p1 p2) ctx
        o = getDrawingObject (g p1 p2) ctx 
    in (a,o)
