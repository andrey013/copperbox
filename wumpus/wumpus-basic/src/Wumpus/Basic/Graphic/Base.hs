{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Graphic.Base
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
-- Base classes for monadic drawing.
--
-- Note on suffix names. Function types suffixed @F@ are functions 
-- from same-to-same, e.g.:
--
-- > type Point2F u = Point2 u -> Point2 u
--
-- Functional types subfixed @R@ are functions from some static 
-- context to the answer type (c.f the ReaderMonad), e.g.:
--
-- > newtype DrawingR a = DrawingR { getDrawingR :: DrawingContext -> a }
--
-- The suffix @M@ is used for classes defining monadic actions.
--
-- \*\* WARNING \*\* - some names are expected to change 
-- particularly the naming of the /append/ and /concat/ functions.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic.Base
  (
    
  -- * Drawing monads.
    MonUnit
  , TraceM(..)
  , DrawingCtxM(..)
  , asksDC

  , ScalingM(..)
  , DirectionM(..)
 
  , PointSupplyM(..)

  -- * Base types
  , HPrim
  , hprimToList
  , singleH

  , Point2F
  , DPoint2F

  , DrawingR
  , LocDrawingR
  , DLocDrawingR

  , runDrawingR

  , Graphic
  , DGraphic
  

  , runGraphic
  , xlinkGraphic

  , LocGraphic
  , DLocGraphic
  , localLG
  , lgappend

  , Image
  , DImage
  , LocImage
  , DLocImage

  , runImage
  , intoImage
  , intoLocImage
  , xlinkImage

  , ConnDrawingR
  , DConnDrawingR
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
import Data.Monoid


--------------------------------------------------------------------------------
-- Monadic drawing

-- | DUnit is always for fully saturated type constructors, so 
-- (seemingly) an equivalent type family is needed for monads.

type family MonUnit m :: * 


-- | Collect elementary graphics as part of a larger drawing.
--
-- TraceM works much like a writer monad.
--
class Monad m => TraceM (m :: * -> *) where
  trace  :: HPrim (MonUnit m) -> m ()

class Monad m => DrawingCtxM (m :: * -> *) where
  askDC    :: m DrawingContext
  localize :: (DrawingContext -> DrawingContext) -> m a -> m a


-- | Project a value out of a context.
--
asksDC :: DrawingCtxM m => (DrawingContext -> a) -> m a
asksDC f = askDC >>= (return . f)


-- | Scaling...
--
class Monad m => ScalingM m where
  type XDim m :: *
  type YDim m :: *
  scaleX :: (u ~ MonUnit m, ux ~ XDim m) => ux -> m u
  scaleY :: (u ~ MonUnit m, uy ~ YDim m) => uy -> m u
  scalePt  :: (u ~ MonUnit m, ux ~ XDim m, uy ~ YDim m) 
           => ux -> uy -> m (Point2 u)
  scaleVec :: (u ~ MonUnit m, ux ~ XDim m, uy ~ YDim m) 
           => ux -> uy -> m (Vec2 u)



-- Should this use MonUnit for consistency ??

class Monad m => DirectionM m where
  localTheta    :: Radian -> m a -> m a
  asksTheta     :: (Radian -> a) -> m a 
  parallel      :: Floating u => u -> m (Vec2 u)
  perpendicular :: Floating u => u -> m (Vec2 u)



-- | A monad that supplies points, e.g. a turtle monad. 
--
class Monad m => PointSupplyM (m :: * -> *) where
  position :: u ~ MonUnit m => m (Point2 u)
 

--------------------------------------------------------------------------------
-- Base types

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
newtype HPrim u = HPrim { getHPrim :: H (PrimElement u) }

-- Note - only a Monoid instance for HPrim - they cannot be 
-- shown, fmapped etc.

instance Monoid (HPrim u) where
  mempty          = HPrim emptyH
  ha `mappend` hb = HPrim $ getHPrim ha `appendH` getHPrim hb


hprimToList :: HPrim u -> [PrimElement u]
hprimToList = toListH . getHPrim


singleH :: PrimElement u -> HPrim u
singleH = HPrim . wrapH 

-- | Point transformation function.
--
type Point2F u = Point2 u -> Point2 u

type DPoint2F = Point2F Double

--------------------------------------------------------------------------------
--

-- | Drawings in Wumpus-Basic have an implicit /graphics state/ 
-- the @DrawingContext@, the most primitive building block is 
-- a function from the DrawingContext to some polymorphic answer.
-- 
-- This functional type is represented concretely as @DrawingR@.
-- 
-- > DrawingR :: DrawingContext -> a 
--
newtype DrawingR a = DrawingR { getDrawingR :: DrawingContext -> a }


instance Functor DrawingR where
  fmap f ma = DrawingR $ \ctx -> f $ getDrawingR ma ctx 

-- The monoid instance seems sensible...
--
instance Monoid a => Monoid (DrawingR a) where 
  mempty          = DrawingR $ \_   -> mempty
  fa `mappend` fb = DrawingR $ \ctx -> 
                      getDrawingR fa ctx `mappend` getDrawingR fb ctx

-- Applicative

instance Applicative DrawingR where
  pure a    = DrawingR $ \_   -> a
  mf <*> ma = DrawingR $ \ctx -> let f = getDrawingR mf ctx
                                     a = getDrawingR ma ctx
                                 in f a

-- Monad 

instance Monad DrawingR where
  return a  = DrawingR $ \_   -> a
  ma >>= k  = DrawingR $ \ctx -> let a = getDrawingR ma ctx
                                 in (getDrawingR . k) a ctx 


instance DrawingCtxM DrawingR where
  askDC           = DrawingR $ \ctx -> ctx
  localize upd df = DrawingR $ \ctx -> getDrawingR df (upd ctx)
  

-- | Run a /Drawing Function/ with the supplied /Drawing Context/.
--
runDrawingR :: DrawingContext -> DrawingR a -> a
runDrawingR ctx df = getDrawingR df ctx




type LocDrawingR u a = Point2 u -> DrawingR a 

type DLocDrawingR a = LocDrawingR Double a


--------------------------------------------------------------------------------


-- Simple drawing - representing one or more prims

type Graphic u = DrawingR (HPrim u)

type DGraphic = Graphic Double


runGraphic :: DrawingContext -> Graphic u -> HPrim u
runGraphic ctx gf = (getDrawingR gf) ctx


xlinkGraphic :: XLink -> Graphic u -> Graphic u
xlinkGraphic xlink gf = DrawingR $ \ctx -> 
    let xs = hprimToList $ runGraphic ctx gf 
    in (singleH $ xlinkGroup xlink xs)


--------------------------------------------------------------------------------


-- | Commonly graphics take a start point as well as a drawing 
-- context.
-- 
-- Here they are called a LocGraphic - graphic with a (starting) 
-- location.
--
type LocGraphic u = Point2 u -> Graphic u

type DLocGraphic = LocGraphic Double


localLG :: 
    (DrawingContext -> DrawingContext) -> LocGraphic u -> LocGraphic u
localLG upd img = \pt -> localize upd (img pt) 


-- | Composition operator for LocGraphic - both LocGraphics
-- are drawn at the same origin and the results concatenated.
--
--
lgappend :: LocGraphic u -> LocGraphic u -> LocGraphic u
lgappend f g = \pt -> f pt `mappend` g pt


--------------------------------------------------------------------------------


-- | Images return a value as well as drawing. A /node/ is a 
-- typical example - nodes are drawing but the also support 
-- taking anchor points.
--
type Image u a = DrawingR (a, HPrim u)

type DImage a = Image Double a

type LocImage u a = Point2 u -> Image u a

type DLocImage a = LocImage Double a

runImage :: DrawingContext -> Image u a -> (a,HPrim u)
runImage ctx img = (getDrawingR img) ctx


intoImage :: DrawingR a -> Graphic u -> Image u a
intoImage f g = DrawingR $ \ctx -> 
    let a = getDrawingR f ctx; o = getDrawingR g ctx in (a,o)


intoLocImage :: LocDrawingR u a -> LocGraphic u -> LocImage u a
intoLocImage f g pt = DrawingR $ \ctx -> 
    let a = getDrawingR (f pt) ctx; o = getDrawingR (g pt) ctx in (a,o)


xlinkImage :: XLink -> Image u a -> Image u a
xlinkImage xlink img = DrawingR $ \ctx -> 
    let (a,hp) = runImage ctx img 
    in (a, singleH $ xlinkGroup xlink $ hprimToList hp)

--------------------------------------------------------------------------------
--

type ConnDrawingR u a = Point2 u -> Point2 u -> DrawingR a

type DConnDrawingR a = ConnDrawingR Double a

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


intoConnImage :: ConnDrawingR u a -> ConnGraphic u -> ConnImage u a
intoConnImage f g p1 p2 = DrawingR $ \ctx -> 
    let a = getDrawingR (f p1 p2) ctx; o = getDrawingR (g p1 p2) ctx in (a,o)



