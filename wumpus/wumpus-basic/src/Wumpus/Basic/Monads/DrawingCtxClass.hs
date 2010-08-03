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


  , strokeAttr
  , fillAttr
  , textAttr
  , markHeight
  
  ) where

import Wumpus.Basic.Graphic.DrawingAttr ( DrawingAttr(..), standardAttr )
import qualified Wumpus.Basic.Graphic.DrawingAttr as DA

import Wumpus.Core                      -- package: wumpus-core

import Control.Monad


-- local to add? or new class...

class Monad m => DrawingCtxM m where
  askDrawingCtx :: m DrawingAttr


strokeAttr  :: DrawingCtxM m => m (DRGB, StrokeAttr)
strokeAttr  = liftM DA.strokeAttr askDrawingCtx


fillAttr    :: DrawingCtxM m => m DRGB
fillAttr    = liftM DA.fillAttr askDrawingCtx

textAttr    :: DrawingCtxM m => m  (DRGB, FontAttr)
textAttr    = liftM DA.textAttr askDrawingCtx

markHeight  :: (Fractional u, DrawingCtxM m) => m u
markHeight  = liftM DA.markHeight askDrawingCtx

