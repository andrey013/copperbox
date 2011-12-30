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
  , prim1
  , catToList
  , cpmap

  , HPrim
  , hprimToList
  , singleH

  ) where

import ZMidi.Basic.Kernel.Base.BaseDefs

import ZMidi.Basic.Utils.HList
import ZMidi.Basic.Utils.JoinList ( JoinList, ViewR(..), viewr )
import qualified ZMidi.Basic.Utils.JoinList as JL

import ZMidi.Core                               -- package: zmidi-core

import Data.Monoid

type Primitive = (OnsetTime, MidiEvent)

data CatPrim = CZero
             | Cat1 (JoinList Primitive)


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

catToList :: CatPrim -> [Primitive]
catToList CZero    = []
catToList (Cat1 a) = JL.toList a


prim1 :: (OnsetTime,MidiVoiceEvent) -> CatPrim
prim1 (ot,e) = Cat1 $ JL.one (ot, VoiceEvent e)


-- | Map 
--
cpmap :: (Primitive -> Primitive) -> CatPrim -> CatPrim
cpmap _ CZero    = CZero
cpmap f (Cat1 a) = Cat1 $ fmap f a

--------------------------------------------------------------------------------
-- Lists of Primitives

-- | Musical objects, such as arpegios need more than one 
-- primitive (note) for their construction. Hence, the primary 
-- representation to build musical objects upon must support 
-- /concatenation/ of primitives. 
-- 
newtype HPrim u = HPrim { getHPrim :: H Primitive }

-- Note - only a Monoid instance for HPrim - they cannot be 
-- shown, fmapped etc.

instance Monoid (HPrim u) where
  mempty          = HPrim emptyH
  ha `mappend` hb = HPrim $ getHPrim ha `appendH` getHPrim hb

  mconcat []      = mempty
  mconcat (a:as)  = step a as
    where
      step ac []     = ac
      step ac (x:xs) = step (ac `mappend` x) xs


-- | Extract the internal list of 'Primitive' from a 'HPrim'.
--
hprimToList :: HPrim u -> [Primitive]
hprimToList = toListH . getHPrim


-- | Form a 'HPrim' from a 'CatPrim'.
--
singleH :: CatPrim -> HPrim u
singleH CZero    = HPrim emptyH
singleH (Cat1 a) = HPrim $ step emptyH (viewr a) 
  where
    step ac EmptyR    = ac
    step ac (se :> e) = step (e `consH` ac) (viewr se)




