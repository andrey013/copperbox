{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
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
-- The common base drawing objects in Wumpus-Basic - a 
-- semigroup class, monad classes (TraceM analogue to Writer,
-- DrawingCtxM analogue to Reader), a wrapped Hughes list of
-- primitives. 
-- 
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic.Base
  (

  -- A semigroup class.
    OPlus(..)
  , oconcat
  , anterior    
  , superior

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

  , PrimGraphic
  , getPrimGraphic
  , primGraphic
  , collectH

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

anterior :: OPlus t => t -> (t -> t)
anterior a = (a `oplus`)

superior :: OPlus t => t -> (t -> t)
superior a = (`oplus` a)




-- Note - this produces tall-skinny trees in Wumpus-core.
-- This does not impact on the generated PostScript but it is 
-- (probably) inefficient for traversals in Wumpus.
--
-- There is scope to modify the Primitive type in Wumpus-Core 
-- (make Group indepenent of XLink) so wider trees can be made.

instance OPlus (Primitive u) where
  a `oplus` b = primGroup [a,b]

instance (OPlus a, OPlus b) => OPlus (a,b) where
  (a,b) `oplus` (a',b') = (a `oplus` a', b `oplus` b')


instance OPlus a => OPlus (r -> a) where
  f `oplus` g = \x -> f x `oplus` g x

-- The functional instance (r -> a) also covers (r1 -> r2 -> a),
-- (r1 -> r2 -> r3 -> a) etc.


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
-- Lists of primitives...

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


-- Affine transformations

instance (Real u, Floating u) => Rotate (PrimGraphic u) where
  rotate ang = PrimGraphic . rotate ang . getPrimGraphic


instance (Real u, Floating u) => RotateAbout (PrimGraphic u) where
  rotateAbout ang pt = PrimGraphic . rotateAbout ang pt . getPrimGraphic


instance Num u => Scale (PrimGraphic u) where
  scale sx sy = PrimGraphic . scale sx sy . getPrimGraphic


instance Num u => Translate (PrimGraphic u) where
  translate dx dy = PrimGraphic . translate dx dy . getPrimGraphic


primGraphic :: Primitive u -> PrimGraphic u 
primGraphic = PrimGraphic

collectH :: PrimGraphic u -> HPrim u
collectH = singleH . getPrimGraphic

