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

  -- * Re-exports from Wumpus.Basic.Graphic.DrawingAttr
    DrawingAttr(..)
  , standardAttr

  -- * DrawingCtx class

  , DrawingCtxM(..)
  , withinModifiedCtx

  , strokeAttr
  , fillAttr
  , textAttr
  , markHeight

  , textDimensions
  
  ) where

import Wumpus.Basic.Graphic.DrawingAttr ( DrawingAttr(..), standardAttr )
import qualified Wumpus.Basic.Graphic.DrawingAttr as DA

import Wumpus.Core                      -- package: wumpus-core

import Control.Monad


-- local to add? or new class...

class Monad m => DrawingCtxM m where
  askDrawingCtx :: m DrawingAttr
  localCtx      :: DrawingAttr -> m a -> m a



withinModifiedCtx :: DrawingCtxM m 
                  => (DrawingAttr -> DrawingAttr) -> m a -> m a
withinModifiedCtx upd ma = askDrawingCtx >>= \ctx -> localCtx (upd ctx) ma


strokeAttr  :: DrawingCtxM m => m (DRGB, StrokeAttr)
strokeAttr  = liftM DA.strokeAttr askDrawingCtx


fillAttr    :: DrawingCtxM m => m DRGB
fillAttr    = liftM DA.fillAttr askDrawingCtx

textAttr    :: DrawingCtxM m => m (DRGB, FontAttr)
textAttr    = liftM DA.textAttr askDrawingCtx

markHeight  :: (FromPtSize u, DrawingCtxM m) => m u
markHeight  = liftM (DA.markHeight) askDrawingCtx

textDimensions :: (FromPtSize u, DrawingCtxM m) => String -> m (u,u)
textDimensions str = liftM (DA.textDimensions str) askDrawingCtx

