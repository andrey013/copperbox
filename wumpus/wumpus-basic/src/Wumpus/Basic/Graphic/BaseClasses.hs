{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Graphic.BaseClasses
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Base classes and type families.
--
-- Drawing is always built on TraceM and DrawingCtxM, it may use
-- PointSupplyM for chains or turtle drawing.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic.BaseClasses
  (
    MonUnit
  , TraceM(..)
  , DrawingCtxM(..)
  , asksCtx  

  , PointSupplyM(..)

  ) where

import Wumpus.Basic.Graphic.BaseTypes ( HPrim )
import Wumpus.Basic.Graphic.DrawingContext
 

import Wumpus.Core                              -- package: wumpus-core




-- DUnit is always for fully saturated types, so (seemingly) an equivalent
-- type family is needed.

type family MonUnit m :: * 


-- | Collect elementary graphics as part of a larger drawing.
--
-- TraceM works much like a writer monad.
--
class Monad m => TraceM (m :: * -> *) where
  trace  :: HPrim (MonUnit m) -> m ()

class Monad m => DrawingCtxM (m :: * -> *) where
  askCtx    :: m DrawingContext
  localCtx  :: (DrawingContext -> DrawingContext) -> m a -> m a


-- | Project a value out of a context.
--
asksCtx :: DrawingCtxM m => (DrawingContext -> a) -> m a
asksCtx f = askCtx >>= (return . f)


-- | A monad that supplies points, e.g. a turtle monad. 
--
class Monad m => PointSupplyM (m :: * -> *) where
  position :: u ~ MonUnit m => m (Point2 u)
 
