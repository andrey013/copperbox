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




  ) where

import Wumpus.Basic.Utils.JoinList

import Wumpus.Core                      -- package: wumpus-core


import Data.Monoid







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
newtype HPrim u = HPrim { getHPrim :: JoinList Primitive }

-- Note - only a Monoid instance for HPrim - they cannot be 
-- shown, fmapped etc.

instance Monoid (HPrim u) where
  mempty          = HPrim mempty
  ha `mappend` hb = HPrim $ getHPrim ha `mappend` getHPrim hb


-- instance Functor HPrim where
--   fmap f = HPrim . fmap (fmap f) . getHPrim




hprimToList :: HPrim u -> [Primitive]
hprimToList = toList . getHPrim


singleH :: Primitive -> HPrim u
singleH = HPrim . one 






