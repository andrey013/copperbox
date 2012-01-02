{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Basic.Kernel.Base.WrappedPrimitive
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Wrapped Primitives supporting concatenation.
--
--------------------------------------------------------------------------------

module ZMidi.Basic.Kernel.Base.WrappedPrimitive
  ( 

    Primitive
  , CatPrim
  , primI
  , primOO
  , catToEventList
  , cpmap


  ) where

import ZMidi.Basic.Kernel.Base.BaseDefs
import ZMidi.Basic.Primitive.Syntax
import ZMidi.Basic.Primitive.Transform

import ZMidi.Basic.Utils.JoinList ( JoinList )
import qualified ZMidi.Basic.Utils.JoinList as JL

import ZMidi.Core                               -- package: zmidi-core

import Data.Monoid



-- | @Primitive@ doesn\'t support cheap concat.
-- 
data CatPrim = CZero
             | Cat1 (JoinList Primitive)

type instance DUnit CatPrim = Double


instance Translate CatPrim where
  translate dt = cpmap (translate dt)

instance SReverse CatPrim where
  sreverse = cpmap sreverse

instance Scale CatPrim where
  scale sx = cpmap (scale sx)

instance Reposition CatPrim where
  reposition ot = cpmap (reposition ot)

--
-- Note - the concatenation is sequential. 
-- 
-- The duration of the last element of the first list is used to 
-- delay the first element of the second list.
--
instance Monoid CatPrim where
  mempty                  = CZero
  CZero  `mappend` b      = b
  a      `mappend` CZero  = a
  Cat1 a `mappend` Cat1 b = Cat1 $ a `mappend` b 

catToEventList :: CatPrim -> EventList
catToEventList (Cat1 se) = frame se
catToEventList _         = frame mempty


--
-- Actually this is a bit crummy, we are only supporting 1 event 
-- long primitives when primitive can support many events.
-- 
-- Thus we are losing the cheap transformation that Primitive is 
-- designed for.
--


primI :: OnsetTime -> MidiVoiceEvent -> CatPrim
primI ot e = Cat1 $ JL.one $ eventGroup ot [(0, instant $ VoiceEvent e)]

primOO :: OnsetTime -> MidiVoiceEvent -> Double -> MidiVoiceEvent -> CatPrim
primOO ot e0 drn e1 = Cat1 $ JL.one $ eventGroup ot [(0, prim)]
  where
    prim = onoff (VoiceEvent e0) drn (VoiceEvent e1)



-- | Map 
--
cpmap :: (Primitive -> Primitive) -> CatPrim -> CatPrim
cpmap _ CZero    = CZero
cpmap f (Cat1 a) = Cat1 $ fmap f a

