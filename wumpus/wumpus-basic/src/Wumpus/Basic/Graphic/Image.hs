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
-- Iamge types.
--
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic.Image
  (
    HPrim
  , Point2T
  , DrawingObject(..)
  , Image

  , appendImage
  
  , asksObj
  , localCtxObj

  , runImage

  , LocImage

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

type Image u = DrawingObject (HPrim u)



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

appendImage :: Image u -> Image u -> Image u
appendImage img1 img2 = DrawingObject $ \ctx ->          
      (getDrawingObject img1 ctx) `appendH` (getDrawingObject img2 ctx)



asksObj :: (DrawingContext -> a) -> DrawingObject a
asksObj fn = DrawingObject $ \ctx -> fn ctx

localCtxObj :: (DrawingContext -> DrawingContext) 
            -> DrawingObject a -> DrawingObject a
localCtxObj upd img = DrawingObject $ \ctx -> getDrawingObject img (upd ctx)

runImage :: DrawingContext -> Image u -> HPrim u
runImage ctx img = (getDrawingObject img) ctx


--------------------------------------------------------------------------------




-- | Commonly images take a start point as well as a drawing 
-- context.
-- 
-- Here they are called a LocImage - image with a (starting) 
-- location.
--
type LocImage u = Point2 u -> Image u


