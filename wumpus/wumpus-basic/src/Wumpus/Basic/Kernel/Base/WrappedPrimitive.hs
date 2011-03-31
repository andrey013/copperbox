{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Base.WrappedPrimitive
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Listified version of the Primitive type from Wumpus-Core.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Base.WrappedPrimitive
  (



  -- * Primitives
    CatPrim
  , prim1
  , cpmap

  , HPrim
  , hprimToList
  , singleH




  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Utils.HList

import Wumpus.Core                      -- package: wumpus-core


import Data.Monoid



-- | CatPrim could probably manage happily just being a
-- Primitive, but it is wrapped as a newtype...
--
newtype CatPrim = CatPrim { getCatPrim :: Primitive }

type instance DUnit CatPrim = Double

instance OPlus CatPrim where
  a `oplus` b = CatPrim $ getCatPrim a `primCat` getCatPrim b



instance Rotate CatPrim where
  rotate ang = CatPrim . rotate ang . getCatPrim

instance RotateAbout CatPrim where
  rotateAbout pt ang = CatPrim . rotateAbout pt ang . getCatPrim

instance Scale CatPrim where
  scale sx sy = CatPrim . scale sx sy . getCatPrim

instance Translate CatPrim where
  translate dx dy = CatPrim . translate dx dy . getCatPrim


prim1 :: Primitive -> CatPrim 
prim1 = CatPrim

cpmap :: (Primitive -> Primitive) -> CatPrim -> CatPrim
cpmap f  = CatPrim . f . getCatPrim

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
-- NOTE - currently HPrim has a phantom unit @u@, this is so 
-- trace drawings can have a unit type, but this may change as 
-- perhaps trace drawings don\'t benefit from having units.
--

newtype HPrim u = HPrim { getHPrim :: H Primitive }

-- Note - only a Monoid instance for HPrim - they cannot be 
-- shown, fmapped etc.

instance Monoid (HPrim u) where
  mempty          = HPrim emptyH
  ha `mappend` hb = HPrim $ getHPrim ha `appendH` getHPrim hb


hprimToList :: HPrim u -> [Primitive]
hprimToList = toListH . getHPrim


singleH :: CatPrim -> HPrim u
singleH = HPrim . wrapH . getCatPrim 






