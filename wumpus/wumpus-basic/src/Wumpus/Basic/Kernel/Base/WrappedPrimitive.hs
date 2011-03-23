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



-- | To enable monadic operations on graphic objects such as Image 
-- we need primtives to support @empty@ and @append@ like the 
-- collector of a writer monad.
--
data CatPrim = PZero | PCat Primitive

instance Monoid CatPrim where
  mempty                  = PZero
  PZero  `mappend` b      = b
  a      `mappend` PZero  = a
  PCat a `mappend` PCat b = PCat $ a `primCat` b


instance OPlus CatPrim where
  oplus = mappend



instance DRotate CatPrim where
  drotate ang (PCat a) = PCat $ drotate ang a
  drotate _   a        = a

instance DRotateAbout CatPrim where
  drotateAbout pt ang (PCat a) = PCat $ drotateAbout pt ang a
  drotateAbout _  _   a        = a

instance DScale CatPrim where
  dscale sx sy (PCat a) = PCat $ dscale sx sy a
  dscale _  _  a        = a

instance DTranslate CatPrim where
  dtranslate dx dy (PCat a) = PCat $ dtranslate dx dy a
  dtranslate _  _  a        = a


prim1 :: Primitive -> CatPrim 
prim1 = PCat

cpmap :: (Primitive -> Primitive) -> CatPrim -> CatPrim
cpmap _ PZero    = PZero
cpmap f (PCat a) = PCat $ f a

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
singleH PZero    = mempty 
singleH (PCat a) = HPrim $ wrapH a






