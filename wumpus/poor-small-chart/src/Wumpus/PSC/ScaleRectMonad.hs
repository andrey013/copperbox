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
  , xRange
  , yRange
  , borderOrigin
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


data RConfig ux uy = RConfig 
      { rcfg_rect       :: DRectangleLoc
      , rcfg_xrange     :: Range ux
      , rcfg_yrange     :: Range uy
      }


-- Specialise the output unit to Double, the loss of 
-- generality at least makes for better type signatures.
--

newtype ScaleRectM ux uy a = 
            ScaleRectM { getScaleRectM ::  CoordScaleT ux uy Double
                                         ( ReaderT (RConfig ux uy) Id) a }


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


runScaleRectM :: (Range ux, ux -> Double) 
              -> (Range uy, uy -> Double)  
              -> DRectangleLoc 
              -> ScaleRectM ux uy a -> a
runScaleRectM (xrange,xf) (yrange,yf) rect mf = 
      runId 
    ( runReaderT rconfig
    ( runCoordScaleT scale_ctx $ getScaleRectM mf ))
  where
     scale_ctx   = rectangleScaleCtx (xrange, xf) (yrange,yf) (fst rect)
     rconfig     = RConfig { rcfg_rect       = rect
                           , rcfg_xrange     = xrange
                           , rcfg_yrange     = yrange
                           }



askRConfig :: ScaleRectM ux uy (RConfig ux uy)
askRConfig = ScaleRectM $ lift $ ask


xRange :: ScaleRectM ux uy (Range ux)
xRange = rcfg_xrange <$> askRConfig


yRange :: ScaleRectM ux uy (Range uy)
yRange = rcfg_yrange <$> askRConfig


borderRectangle :: ScaleRectM ux uy DRectangleLoc
borderRectangle = rcfg_rect <$> askRConfig

borderOrigin :: ScaleRectM ux uy DPoint2
borderOrigin = snd <$> borderRectangle

borderWidth :: ScaleRectM ux uy Double
borderWidth = (rect_width . fst) <$> borderRectangle


borderHeight :: ScaleRectM ux uy Double
borderHeight = (rect_height . fst) <$> borderRectangle

withinBorderRect :: DPoint2 -> ScaleRectM ux uy Bool
withinBorderRect pt = (withinRectangleLoc pt) <$> borderRectangle

