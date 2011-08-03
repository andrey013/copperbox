{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Basic.Kernel.Base.WrappedPrimitive
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

module Majalan.Basic.Kernel.Base.WrappedPrimitive
  ( 
    CatPrim
  , prim1
  , cpmap

  , HPrim
  , hprimToList
  , hprimToScoreMb
  , singleH

  ) where

import Majalan.Basic.Utils.HList
import Majalan.Basic.Utils.JoinList ( JoinList, ViewR(..), viewr )
import qualified Majalan.Basic.Utils.JoinList as JL

import Majalan.Core                             -- package: majalan-core

import Data.Monoid


-- NOTE - we could probably live with just one representation...

data CatPrim = CZero
             | Cat1 (JoinList Note)


instance Monoid CatPrim  where
  mempty                  = CZero
  CZero  `mappend` b      = b
  a      `mappend` CZero  = a
  Cat1 a `mappend` Cat1 b = Cat1 $ a `mappend` b 



prim1 :: Note -> CatPrim
prim1 = Cat1 . JL.one


-- | Map 
--
cpmap :: (Note -> Note) -> CatPrim -> CatPrim
cpmap _ CZero    = CZero
cpmap f (Cat1 a) = Cat1 $ fmap f a

--------------------------------------------------------------------------------
-- Lists of Primitives

-- | Musical objects, such as arpegios need more than one 
-- primitive (note) for their construction. Hence, the primary 
-- representation to build musical objects upon must support 
-- /concatenation/ of primitives. 
-- 
newtype HPrim u = HPrim { getHPrim :: H Note }

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


-- | Extract the internal list of 'Event' from a 'HPrim'.
--
hprimToList :: HPrim u -> [Note]
hprimToList = toListH . getHPrim


-- | Promotion of @HPrim@ to @RScore@.
-- 
-- Represented as a Maybe because empty Scores cannot be rendered
-- so client code must deal with this case.
-- 
hprimToScoreMb :: HPrim u -> Maybe RScore
hprimToScoreMb hf = let prims = hprimToList hf in 
                    if null prims then Nothing else Just (frame prims)



-- | Form a 'HPrim' from a 'CatPrim'.
--
singleH :: CatPrim -> HPrim u
singleH CZero    = HPrim emptyH
singleH (Cat1 a) = HPrim $ step emptyH (viewr a) 
  where
    step ac EmptyR    = ac
    step ac (se :> e) = step (e `consH` ac) (viewr se)




