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
-- Notes on prefix and suffix names:
--
-- Function types suffixed @F@ are functions from same-to-same, e.g.:
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
-- The prefix @Loc@ indicates a functional type 
-- /from Point2 to something.../
-- 
-- The prefix @ThetaLoc@ indicates a functional type 
-- /from Direction (radian) then Point to something.../
--
-- \*\* WARNING \*\* - some names are expected to change.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic.Base
  (

  -- A semigroup class.
    OPlus(..)
  , oconcat
    
  -- * Drawing monads.
  , MonUnit
  , TraceM(..)
  , DrawingCtxM(..)
  , asksDC
 
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

  , PrimGraphic
  , getPrimGraphic
  , wrapPrim

  , collectH

  , Graphic
  , DGraphic
  , GraphicF  

  , runGraphic
  , xlinkGraphic

  , LocGraphic
  , DLocGraphic

  , Image
  , DImage
  , LocImage
  , DLocImage

  , runImage
  , intoImage
  , intoLocImage
  , xlinkImage

  , ConnectorDrawingR
  , DConnectorDrawingR
  , ConnectorGraphic
  , DConnectorGraphic
  , ConnectorImage
  , DConnectorImage

  , intoConnectorImage

  , ThetaLocDrawingR
  , DThetaLocDrawingR
  , ThetaLocGraphic
  , DThetaLocGraphic
  , ThetaLocImage
  , DThetaLocImage

  ) where

import Wumpus.Basic.Graphic.DrawingContext
import Wumpus.Basic.Utils.HList

import Wumpus.Core                      -- package: wumpus-core


import Control.Applicative
import Data.Monoid



infixr 6 `oplus`

-- | A Semigroup class.
-- 
class OPlus t where
  oplus :: t -> t -> t

oconcat :: OPlus t => t -> [t] -> t
oconcat t = step t
  where
    step ac []     = ac
    step ac (x:xs) = step (ac `oplus` x) xs


-- Note - this produces tall-skinny trees in Wumpus-core.
-- This does not impact on the generated PostScript but it is 
-- (probably) inefficient for traversals in Wumpus.
--
-- There is scope to modify the Primitive type in Wumpus-Core 
-- (make Group indepenent of XLink) so wider trees can be made.

instance OPlus (Primitive u) where
  oplus a b = primGroup [a,b]

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

class (Applicative m, Monad m) => DrawingCtxM (m :: * -> *) where
  askDC    :: m DrawingContext
  localize :: (DrawingContext -> DrawingContext) -> m a -> m a


-- | Project a value out of a context.
--
asksDC :: DrawingCtxM m => (DrawingContext -> a) -> m a
asksDC f = askDC >>= (return . f)






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
newtype HPrim u = HPrim { getHPrim :: H (Primitive u) }

-- Note - only a Monoid instance for HPrim - they cannot be 
-- shown, fmapped etc.

instance Monoid (HPrim u) where
  mempty          = HPrim emptyH
  ha `mappend` hb = HPrim $ getHPrim ha `appendH` getHPrim hb


hprimToList :: HPrim u -> [Primitive u]
hprimToList = toListH . getHPrim


singleH :: Primitive u -> HPrim u
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


instance OPlus a => OPlus (DrawingR a)  where
  fa `oplus` fb = DrawingR $ \ctx -> 
                      getDrawingR fa ctx `oplus` getDrawingR fb ctx

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

-- As of version 0.36.0, Wumpus-Core supports grouping primitives
-- together (a common operation in vector drawing editors). 
--
-- For Wumpus-Basic this means e.g. a line with arrowheads can 
-- still be a primitive.
--
-- Still, we wrap Primitive as a newtype...
--

newtype PrimGraphic u = PrimGraphic { getPrimGraphic :: Primitive u }
  deriving (Eq,Show)

type instance DUnit (PrimGraphic u) = u

instance OPlus (PrimGraphic u) where
  oplus a b = PrimGraphic $ getPrimGraphic a `oplus` getPrimGraphic b

instance (Real u, Floating u) => Rotate (PrimGraphic u) where
  rotate ang = PrimGraphic . rotate ang . getPrimGraphic


instance (Real u, Floating u) => RotateAbout (PrimGraphic u) where
  rotateAbout ang pt = PrimGraphic . rotateAbout ang pt . getPrimGraphic


instance Num u => Scale (PrimGraphic u) where
  scale sx sy = PrimGraphic . scale sx sy . getPrimGraphic


instance Num u => Translate (PrimGraphic u) where
  translate dx dy = PrimGraphic . translate dx dy . getPrimGraphic


wrapPrim :: Primitive u -> PrimGraphic u 
wrapPrim = PrimGraphic

collectH :: PrimGraphic u -> HPrim u
collectH = singleH . getPrimGraphic



-- Simple drawing - representing one or more prims

type Graphic u = DrawingR (PrimGraphic u)

type DGraphic = Graphic Double


runGraphic :: DrawingContext -> Graphic u -> PrimGraphic u
runGraphic ctx gf = (getDrawingR gf) ctx


xlinkGraphic :: XLink -> Graphic u -> Graphic u
xlinkGraphic xlink gf = DrawingR $ \ctx -> 
    let a = runGraphic ctx gf 
    in PrimGraphic $ xlinkGroup xlink [getPrimGraphic a]


type GraphicF u = Graphic u -> Graphic u

--------------------------------------------------------------------------------


-- | Commonly graphics take a start point as well as a drawing 
-- context.
-- 
-- Here they are called a LocGraphic - graphic with a (starting) 
-- location.
--
type LocGraphic u = Point2 u -> Graphic u

type DLocGraphic = LocGraphic Double




--------------------------------------------------------------------------------


-- | Images return a value as well as drawing. A /node/ is a 
-- typical example - nodes are drawing but the also support 
-- taking anchor points.
--
type Image u a = DrawingR (a, PrimGraphic u)

type DImage a = Image Double a

type LocImage u a = Point2 u -> Image u a

type DLocImage a = LocImage Double a

runImage :: DrawingContext -> Image u a -> (a,PrimGraphic u)
runImage ctx img = (getDrawingR img) ctx


intoImage :: DrawingR a -> Graphic u -> Image u a
intoImage f g = DrawingR $ \ctx -> 
    let a = getDrawingR f ctx; o = getDrawingR g ctx in (a,o)


intoLocImage :: LocDrawingR u a -> LocGraphic u -> LocImage u a
intoLocImage f g pt = DrawingR $ \ctx -> 
    let a = getDrawingR (f pt) ctx; o = getDrawingR (g pt) ctx in (a,o)


xlinkImage :: XLink -> Image u a -> Image u a
xlinkImage xlink img = DrawingR $ \ctx -> 
    let (a,pg) = runImage ctx img 
    in (a, PrimGraphic $ xlinkGroup xlink [getPrimGraphic pg])

--------------------------------------------------------------------------------
--

type ConnectorDrawingR u a = Point2 u -> Point2 u -> DrawingR a

type DConnectorDrawingR a = ConnectorDrawingR Double a

-- | ConnectorGraphic is a connector drawn between two points 
-- contructing a Graphic.
--
type ConnectorGraphic u = Point2 u -> Point2 u -> Graphic u

type DConnectorGraphic = ConnectorGraphic Double

-- | ConImage is a connector drawn between two points 
-- constructing an Image.
--
type ConnectorImage u a = Point2 u -> Point2 u -> Image u a

type DConnectorImage a = ConnectorImage Double a


intoConnectorImage :: ConnectorDrawingR u a 
                   -> ConnectorGraphic u 
                   -> ConnectorImage u a
intoConnectorImage f g p1 p2 = DrawingR $ \ctx -> 
    let a = getDrawingR (f p1 p2) ctx; o = getDrawingR (g p1 p2) ctx in (a,o)


type ThetaLocDrawingR u a = Radian -> LocDrawingR u a 

type DThetaLocDrawingR a = ThetaLocDrawingR Double a

-- | A function from /Radian -\> Point -\> Graphic/...
--
type ThetaLocGraphic u = Radian -> LocGraphic u

type DThetaLocGraphic = ThetaLocGraphic Double

type ThetaLocImage u a = Radian -> LocImage u a

type DThetaLocImage a = ThetaLocImage Double a 




