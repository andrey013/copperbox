{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE KindSignatures             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernal.Base.BaseExts
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Experimental extras for BaseDefs...
-- 
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Base.BaseExts
  (

  -- * Unit interpretation with respect to the current Point size
    InterpretUnit(..)
  , UnitConvert(..)
  , UnitConvertExt(..)
  , uconvertScalar
  , intraMapPoint

  , Hyperlink(..)
  , Annotate(..)
  , IgnoreAns(..)
  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.CtxUnits

import Wumpus.Core                              -- package: wumpus-core


-- This is pencilled to replace CtxSize if the new types are a
-- success.
--

class Num u => InterpretUnit u where
  normalize :: FontSize -> u -> Double
  dinterp   :: FontSize -> Double -> u

instance InterpretUnit Double where
  normalize _ = id
  dinterp   _ = id 

instance InterpretUnit Centimeter where
  normalize _ = toPsDouble 
  dinterp   _ = fromPsDouble

instance InterpretUnit Em where
  normalize = cfSize
  dinterp   = csSize

instance InterpretUnit En where
  normalize = cfSize
  dinterp   = csSize


class UnitConvert t where
  uconvert :: (InterpretUnit u, InterpretUnit u1) => t u -> t u1

class UnitConvertExt t where
  uconvertExt :: (InterpretUnit u, InterpretUnit u1) => FontSize -> t u -> t u1


instance UnitConvertExt Vec2 where
  uconvertExt sz = fmap (uconvertScalar sz)
    
instance UnitConvertExt Point2 where
  uconvertExt sz = fmap (uconvertScalar sz)


instance UnitConvertExt BoundingBox where
  uconvertExt sz = fmap (uconvertScalar sz)


-- | Convert a scalar value from one unit to another.
--
uconvertScalar :: (InterpretUnit u, InterpretUnit u1) => FontSize -> u -> u1
uconvertScalar sz = dinterp sz . normalize sz


-- Helper for defining Affine instances. This function allows 
-- scaling etc to be applied on a Point coerced to a Double then
-- converted back to the original unit. Thus transformations can 
-- work in contextual units.
--
intraMapPoint :: InterpretUnit u 
              => FontSize -> (DPoint2 -> DPoint2) -> Point2 u -> Point2 u
intraMapPoint sz fn (P2 x y) = 
    let P2 x' y' = fn $ P2 (normalize sz x) (normalize sz y)
    in  P2 (dinterp sz x') (dinterp sz y')


class Hyperlink obj where
  hyperlink :: XLink -> obj -> obj


-- Would these be better as parameterized modules?

class Annotate t s | t -> s where 
  annotate  :: t r u -> s u -> t r u
  decorate  :: t r u -> (r u -> s u) -> t r u
    
class IgnoreAns t s | t -> s where 
  ignoreAns :: forall (u :: *) (r :: * -> *). t r u -> s u
  replaceAns :: r1 u ->  t r u -> t r1 u
