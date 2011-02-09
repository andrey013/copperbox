{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Base.WrappedPrimitive
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Two /warpped/ versions of the Primitive type from Wumpus-Core.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Base.WrappedPrimitive
  (

  -- * Primitives
    HPrim
  , hprimToList
  , singleH

  -- * Collect primitives (writer monad) 
  , TraceM(..)

  , PrimGraphic

  , getPrimGraphic
  , primGraphic
  , metamorphPrim
  , collectH

  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Utils.HList

import Wumpus.Core                      -- package: wumpus-core


import Data.Monoid



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


--------------------------------------------------------------------------------

-- | Collect elementary graphics as part of a larger drawing.
--
-- TraceM works much like a writer monad.
--
class TraceM (m :: * -> *) where
  trace  :: u ~ DUnit (m ()) => HPrim u -> m ()





--------------------------------------------------------------------------------
-- instances

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

--------------------------------------------------------------------------------

primGraphic :: Primitive u -> PrimGraphic u 
primGraphic = PrimGraphic

metamorphPrim :: (Primitive u -> Primitive u) -> PrimGraphic u -> PrimGraphic u
metamorphPrim f = primGraphic . f . getPrimGraphic

collectH :: PrimGraphic u -> HPrim u
collectH = singleH . getPrimGraphic

