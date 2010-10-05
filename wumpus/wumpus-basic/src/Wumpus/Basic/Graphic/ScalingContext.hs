{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Graphic.ScalingContext
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Scaling in X and Y
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Graphic.ScalingContext
  (
    ScalingContext(..)

  , Scaling
  , runScaling
  , ScalingT
  , runScalingT

  , regularScalingContext
  , coordinateScalingContext

  , unitX
  , unitY


  ) where

import Wumpus.Basic.Graphic.Base

import Wumpus.Core				-- package: wumpus-core

import Control.Applicative

data ScalingContext ux uy u = ScalingContext
      { scale_in_x  :: ux -> u
      , scale_in_y  :: uy -> u
      }



-- Chains (for example) want a plain monad rather than a transformer.
--
newtype Scaling ux uy u a = Scaling {
          getScaling :: ScalingContext ux uy u -> a }


type instance MonUnit (Scaling ux uy u) = u



instance Functor (Scaling ux uy u) where
  fmap f ma = Scaling $ \ctx -> let a = getScaling ma ctx in f a

instance Applicative (Scaling ux uy u) where
  pure a    = Scaling $ \_   -> a
  mf <*> ma = Scaling $ \ctx -> let f = getScaling mf ctx
                                    a = getScaling ma ctx
             			in (f a)

instance Monad (Scaling ux uy u) where
  return a = Scaling $ \_   -> a
  m >>= k  = Scaling $ \ctx -> let a = getScaling m ctx
    	     	               in (getScaling . k) a ctx



instance ScalingM (Scaling ux uy u) where
  type XDim (Scaling ux uy u) = ux 
  type YDim (Scaling ux uy u) = uy
  scaleX ux       = Scaling $ \ctx -> (scale_in_x ctx) ux
  scaleY uy       = Scaling $ \ctx -> (scale_in_y ctx) uy
  scalePt ux uy   = Scaling $ \ctx -> P2 (scale_in_x ctx ux) (scale_in_y ctx uy)
  scaleVec ux uy  = Scaling $ \ctx -> V2 (scale_in_x ctx ux) (scale_in_y ctx uy)

runScaling :: ScalingContext ux uy u -> Scaling ux uy u a -> a
runScaling ctx sf = (getScaling sf) ctx    




--------------------------------------------------------------------------------
-- Transformer

-- Turtle (for example) wants a transformer so it can use TraceM
-- and DrawingCtxM
--
newtype ScalingT ux uy u m a = ScalingT { 
          getScalingT :: ScalingContext ux uy u -> m a }

type instance MonUnit (ScalingT ux uy u m) = u


instance Monad m => Functor (ScalingT ux uy u m) where
  fmap f ma = ScalingT $ \ctx -> getScalingT ma ctx >>= \a -> return (f a)

instance Monad m => Applicative (ScalingT ux uy u m) where
  pure a    = ScalingT $ \_   -> return a
  mf <*> ma = ScalingT $ \ctx -> getScalingT mf ctx >>= \f -> 
                                 getScalingT ma ctx >>= \a ->
             			 return (f a)

instance Monad m => Monad (ScalingT ux uy u m) where
  return a = ScalingT $ \_   -> return a
  m >>= k  = ScalingT $ \ctx -> getScalingT m ctx >>= \a -> 
    	     	      	     	(getScalingT . k) a ctx


instance Monad m => ScalingM (ScalingT ux uy u m) where
  type XDim (ScalingT ux uy u m) = ux 
  type YDim (ScalingT ux uy u m) = uy
  scaleX ux       = ScalingT $ \ctx -> return $ (scale_in_x ctx) ux
  scaleY uy       = ScalingT $ \ctx -> return $ (scale_in_y ctx) uy
  scalePt ux uy   = ScalingT $ \ctx -> 
                      return $ P2 (scale_in_x ctx ux) (scale_in_y ctx uy)
  scaleVec ux uy  = ScalingT $ \ctx -> 
                      return $ V2 (scale_in_x ctx ux) (scale_in_y ctx uy)



-- Cross instances - needed to run SalingT /locally/ in Drawing.

instance DrawingCtxM m => DrawingCtxM (ScalingT ux uy u m) where
  askDC           = ScalingT $ \_    -> askDC >>= \dctx -> return dctx
  localize upd mf = ScalingT $ \sctx -> localize upd (getScalingT mf sctx)


instance (Monad m, TraceM m, u ~ MonUnit m) => TraceM (ScalingT ux uy u m) where
  trace a  = ScalingT $ \_ -> trace a 




runScalingT :: ScalingContext ux uy u -> ScalingT ux uy u m a -> m a
runScalingT ctx sf = (getScalingT sf) ctx    

--------------------------------------------------------------------------------
-- constructors for scaling context

regularScalingContext :: Num u => u -> ScalingContext u u u
regularScalingContext u = ScalingContext
      { scale_in_x  = (\x -> u*x)
      , scale_in_y  = (\y -> u*y)
      }

coordinateScalingContext :: Num u => u -> u -> ScalingContext Int Int u
coordinateScalingContext sx sy = ScalingContext
      { scale_in_x  = (\x -> sx * fromIntegral x)
      , scale_in_y  = (\y -> sy * fromIntegral y)
      }



--------------------------------------------------------------------------------
-- operations




unitX :: (ScalingM m, Num ux, ux ~ XDim m, u ~ MonUnit m) => m u
unitX = scaleX 1
 
unitY :: (ScalingM m, Num uy, uy ~ YDim m, u ~ MonUnit m) => m u
unitY = scaleY 1

 