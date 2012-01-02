{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Basic.Primitive.Transform
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- 
--
--------------------------------------------------------------------------------

module ZMidi.Basic.Primitive.Transform
  ( 
    DUnit
  , Translate(..)
  
  , SReverse(..)

  , Scale(..)

  , Reposition(..)

  ) where



-- | Some unit of time (duration) usually Double.
--
-- This very useful for reducing the kind of type classes to *.
-- 
-- Then constraints on the Unit type can be declared on the 
-- instances rather than in the class declaration.
-- 
type family DUnit a :: *


-- | Type class for translation.
--
class Translate t where
  translate :: u ~ DUnit t => u -> t -> t


-- | Type class for \"symbolic\" reverse. 
-- 
-- Reversing \"music\" is a symbolic operation rather than a 
-- properly geometric one. As a geometric operation it would 
-- really be a reflection, but a reflection would need
-- a point of reflection.
--
class SReverse t where
  sreverse :: t -> t




--------------------------------------------------------------------------------
-- Scale

-- | Type class for scaling.
--
-- Scale by a scale factor (1.0 is identity).
-- 
class Scale t where
  scale :: Double -> t -> t

--------------------------------------------------------------------------------
-- Reposition

-- | Change the onset time of an /event/.
-- 
class Reposition t where
  reposition :: u ~ DUnit t => u -> t -> t
