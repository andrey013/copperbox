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
-- \*\* WARNING \*\* - some names are expected to change 
-- particularly the naming of the /append/ and /concat/ functions.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic.BaseTypes
  (
    

    HPrim
  , hprimToList
  , singleH

  , Point2T
  , DPoint2T

  , DrawingF
  , LocDrawingF
  , DLocDrawingF

  , runDF
  , pureDF 
  , askDF
  , asksDF
  , localDF

  , Graphic
  , DGraphic
  

  , runGraphic
  , xlinkGraphic

  , LocGraphic
  , DLocGraphic
  , lgappend

  , Image
  , DImage
  , LocImage
  , DLocImage

  , runImage
  , intoImage
  , intoLocImage
  , xlinkImage

  , ConnDrawingF
  , DConnDrawingF
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
type Point2T u = Point2 u -> Point2 u

type DPoint2T = Point2T Double

--------------------------------------------------------------------------------
--

-- | Drawings in Wumpus-Basic have an implicit /graphics state/ 
-- the @DrawingContext@, the most primitive building block is 
-- a function from the DrawingContext to some polymorphic answer.
-- 
-- This functional type is represented concretely as @DrawingF@.
-- 
-- > DrawingF :: DrawingContext -> a 
--
newtype DrawingF a = DrawingF { getDrawingF :: DrawingContext -> a }


instance Functor DrawingF where
  fmap f ma = DrawingF $ \ctx -> f $ getDrawingF ma ctx 

-- The monoid instance seems sensible...
--
instance Monoid a => Monoid (DrawingF a) where 
  mempty          = DrawingF $ \_   -> mempty
  fa `mappend` fb = DrawingF $ \ctx -> 
                      getDrawingF fa ctx `mappend` getDrawingF fb ctx

-- Applicative

instance Applicative DrawingF where
  pure a    = DrawingF $ \_   -> a
  mf <*> ma = DrawingF $ \ctx -> let f = getDrawingF mf ctx
                                     a = getDrawingF ma ctx
                                 in f a

-- Monad 

instance Monad DrawingF where
  return a  = DrawingF $ \_   -> a
  ma >>= k  = DrawingF $ \ctx -> let a = getDrawingF ma ctx
                                 in (getDrawingF . k) a ctx 

-- | Run a /Drawing Function/ with the supplied /Drawing Context/.
--
runDF :: DrawingContext -> DrawingF a -> a
runDF ctx df = getDrawingF df ctx


-- | Wrap a value into a DrawingF.
--
-- Note the value is /pure/ it does depend on the DrawingContext
-- (it is /context free/).
--
pureDF :: a -> DrawingF a
pureDF a = DrawingF $ \ _ctx -> a 

askDF :: DrawingF DrawingContext
askDF = DrawingF id

asksDF :: (DrawingContext -> a) -> DrawingF a
asksDF fn = DrawingF $ \ctx -> fn ctx

localDF :: (DrawingContext -> DrawingContext) 
        -> DrawingF a -> DrawingF a
localDF upd gf = DrawingF $ \ctx -> getDrawingF gf (upd ctx)



type LocDrawingF u a = Point2 u -> DrawingF a 

type DLocDrawingF a = LocDrawingF Double a


--------------------------------------------------------------------------------


-- Simple drawing - representing one or more prims

type Graphic u = DrawingF (HPrim u)

type DGraphic = Graphic Double


runGraphic :: DrawingContext -> Graphic u -> HPrim u
runGraphic ctx gf = (getDrawingF gf) ctx


xlinkGraphic :: XLink -> Graphic u -> Graphic u
xlinkGraphic xlink gf = askDF >>= \ctx -> 
    let xs = hprimToList $ runGraphic ctx gf in pureDF (singleH $ xlinkGroup xlink xs)


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
lgappend :: LocGraphic u -> LocGraphic u -> LocGraphic u
lgappend f g = \pt -> f pt `mappend` g pt


--------------------------------------------------------------------------------


-- | Images return a value as well as drawing. A /node/ is a 
-- typical example - nodes are drawing but the also support 
-- taking anchor points.
--
type Image u a = DrawingF (a, HPrim u)

type DImage a = Image Double a

type LocImage u a = Point2 u -> Image u a

type DLocImage a = LocImage Double a

runImage :: DrawingContext -> Image u a -> (a,HPrim u)
runImage ctx img = (getDrawingF img) ctx


intoImage :: DrawingF a -> Graphic u -> Image u a
intoImage f g = DrawingF $ \ctx -> 
    let a = getDrawingF f ctx; o = getDrawingF g ctx in (a,o)


intoLocImage :: LocDrawingF u a -> LocGraphic u -> LocImage u a
intoLocImage f g pt = DrawingF $ \ctx -> 
    let a = getDrawingF (f pt) ctx; o = getDrawingF (g pt) ctx in (a,o)


xlinkImage :: XLink -> Image u a -> Image u a
xlinkImage xlink img = askDF >>= \ctx -> 
    let (a,hp) = runImage ctx img 
    in pureDF (a, singleH $ xlinkGroup xlink $ hprimToList hp)

--------------------------------------------------------------------------------
--

type ConnDrawingF u a = Point2 u -> Point2 u -> DrawingF a

type DConnDrawingF a = ConnDrawingF Double a

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


intoConnImage :: ConnDrawingF u a -> ConnGraphic u -> ConnImage u a
intoConnImage f g p1 p2 = DrawingF $ \ctx -> 
    let a = getDrawingF (f p1 p2) ctx; o = getDrawingF (g p1 p2) ctx in (a,o)




