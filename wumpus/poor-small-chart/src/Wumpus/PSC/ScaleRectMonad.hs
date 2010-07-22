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
  , withinBorderRect

  ) where

import Wumpus.PSC.Core

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Basic.Graphic                     -- package: wumpus-basic
import Wumpus.Basic.Monads.CoordScaleMonad

import MonadLib                                 -- package: monadlib

import Control.Applicative
import Control.Monad

-- Specialise the output unit to Double, the loss of 
-- generality at least makes for better type signatures.
--

newtype ScaleRectM ux uy a = 
            ScaleRectM { getScaleRectM ::  CoordScaleT ux uy Double
                                         ( ReaderT DRectangleLoc Id) a }


instance Functor (ScaleRectM ux uy) where
  fmap f = ScaleRectM . fmap f . getScaleRectM

instance Applicative (ScaleRectM ux uy) where
  pure a  = ScaleRectM $ return a
  m <*> f = ScaleRectM $ getScaleRectM m `ap` getScaleRectM f 

instance Monad (ScaleRectM ux uy) where
  return a = ScaleRectM $ return a
  m >>= k  = ScaleRectM $ getScaleRectM m >>= getScaleRectM . k
  

instance CoordScaleM (ScaleRectM ux uy) ux uy Double where
  coordScale (u,v) = ScaleRectM $ coordScale (u,v)
  xScale u         = ScaleRectM $ xScale u
  yScale v         = ScaleRectM $ yScale v


runScaleRectM :: ScaleCtx ux uy Double -> DRectangleLoc -> ScaleRectM ux uy a -> a
runScaleRectM scale_ctx rect mf = runId 
                                ( runReaderT rect 
                                ( runCoordScaleT scale_ctx $ getScaleRectM mf ))


borderRectangle :: ScaleRectM ux uy DRectangleLoc
borderRectangle = ScaleRectM $ lift $ ask

borderWidth :: ScaleRectM ux uy Double
borderWidth = liftM (rect_width . fst) borderRectangle


borderHeight :: ScaleRectM ux uy Double
borderHeight = liftM (rect_height . fst) borderRectangle

withinBorderRect :: DPoint2 -> ScaleRectM ux uy Bool
withinBorderRect pt = (withinRectangleLoc pt) <$> borderRectangle
