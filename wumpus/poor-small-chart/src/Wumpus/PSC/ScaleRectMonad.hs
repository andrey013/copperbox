{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.PSC.ScaleRectMonad
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC with TypeFamilies and more
--
-- CoordScale monad over a reader monad with Rectangle context
--
--------------------------------------------------------------------------------

module Wumpus.PSC.ScaleRectMonad
  (
    ScaleRectM

  , runScaleRectM

  , borderRectangle
  , borderWidth
  , borderHeight

  ) where


import Wumpus.Basic.Graphic
import Wumpus.Basic.Monads.CoordScaleMonad

import MonadLib                                 -- package: monadlib

import Control.Applicative
import Control.Monad

newtype ScaleRectM ux uy u a = 
            ScaleRectM { getScaleRectM ::  CoordScaleT ux uy u
                                         ( ReaderT (Rectangle u) Id) a }


instance Functor (ScaleRectM ux uy u) where
  fmap f = ScaleRectM . fmap f . getScaleRectM

instance Applicative (ScaleRectM ux uy u) where
  pure a  = ScaleRectM $ return a
  m <*> f = ScaleRectM $ getScaleRectM m `ap` getScaleRectM f 

instance Monad (ScaleRectM ux uy u) where
  return a = ScaleRectM $ return a
  m >>= k  = ScaleRectM $ getScaleRectM m >>= getScaleRectM . k
  

instance CoordScaleM (ScaleRectM ux uy u) ux uy u where
  coordScale (u,v) = ScaleRectM $ coordScale (u,v)
  xScale u         = ScaleRectM $ xScale u
  yScale v         = ScaleRectM $ yScale v


runScaleRectM :: ScaleCtx ux uy u -> Rectangle u -> ScaleRectM ux uy u a -> a
runScaleRectM scale_ctx rect mf = runId 
                                ( runReaderT rect 
                                ( runCoordScaleT scale_ctx $ getScaleRectM mf ))


borderRectangle :: ScaleRectM ux uy u (Rectangle u)
borderRectangle = ScaleRectM $ lift $ ask


borderWidth :: ScaleRectM ux uy u u
borderWidth = liftM rect_width borderRectangle


borderHeight :: ScaleRectM ux uy u u
borderHeight = liftM rect_height borderRectangle