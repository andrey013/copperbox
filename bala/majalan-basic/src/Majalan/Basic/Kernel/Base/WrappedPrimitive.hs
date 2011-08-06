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
  , moveCatPrim
  , catPrimToScoreMb

  ) where

import Majalan.Basic.Utils.JoinList ( JoinList )
import qualified Majalan.Basic.Utils.JoinList as JL

import Majalan.Core                             -- package: majalan-core

import Data.Monoid





-- Cheap movement?

data CatPrim = C1 (JoinList (Double -> Note))
             | CRec Double CatPrim CatPrim

instance Monoid CatPrim where
  mempty = C1 mempty
  C1 a `mappend` C1 b   = C1 $ a `mappend` b
  ca   `mappend` cb     = CRec 0 ca cb


prim1 :: (Double -> Note) -> CatPrim
prim1 = C1 . JL.one 

moveCatPrim :: Double -> CatPrim -> CatPrim
moveCatPrim x (CRec y ca cb) = CRec (x+y) ca cb
moveCatPrim x ca             = CRec x ca (C1 mempty)


-- | Promotion of @CatPrim@ to @RScore@.
-- 
-- Represented as a Maybe because empty Scores cannot be rendered
-- so client code must deal with this case.
-- 
catPrimToScoreMb :: CatPrim -> Maybe RScore
catPrimToScoreMb = final . step 0 
  where
    step x (C1 fs)        = fmap ($ x) fs
    step x (CRec y ca cb) = mappend (step (x+y) ca) (step (x+y) cb) 

    final sx | JL.null sx  = Nothing
             | otherwise   = Just $ frame $ JL.toList sx


