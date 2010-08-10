{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Monads.DrawingCtxClass
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Class.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Monads.DrawingCtxClass
  (


  -- * DrawingCtx class

    DrawingCtxM(..)
  , withinModifiedCtx
  
  ) where

import Wumpus.Basic.Graphic.DrawingAttr


class Monad m => DrawingCtxM m where
  askDrawingCtx :: m DrawingAttr
  localCtx      :: DrawingAttr -> m a -> m a



withinModifiedCtx :: DrawingCtxM m 
                  => (DrawingAttr -> DrawingAttr) -> m a -> m a
withinModifiedCtx upd ma = askDrawingCtx >>= \ctx -> localCtx (upd ctx) ma


