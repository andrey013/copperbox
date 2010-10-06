{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.PSC.Bivariate
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Handling bivariate data and its projection into the dimensions
-- of a  drawing.
--
--------------------------------------------------------------------------------

module Wumpus.PSC.Bivariate
  (
    Bivariate
  , runBivariate
  , xrange
  , yrange
  , rangeCorners
  , rangeDimensions

  ) where

import Wumpus.PSC.Core

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Basic.Graphic                     -- package: wumpus-basic

import Control.Applicative

-- Bivariate is an extension over the (reader) Scaling Monad with
-- more static arguments for X-drawing-range and Y-drawing-range.
--

newtype Bivariate ux uy u a = Bivariate { 
          getBivariate :: Range ux -> Range uy -> Scaling ux uy u a }

type instance MonUnit (Bivariate ux uy u) = u


instance Functor (Bivariate ux uy u) where
  fmap f ma = Bivariate $ \rux ruy -> 
                getBivariate ma rux ruy >>= \a -> return (f a) 

instance Applicative (Bivariate ux uy u) where
  pure a    = Bivariate $ \_   _   -> return a
  mf <*> ma = Bivariate $ \rux ruy -> 
                getBivariate mf rux ruy >>= \f ->
                getBivariate ma rux ruy >>= \a -> 
                return (f a)

instance Monad (Bivariate ux uy u) where
  return a  = Bivariate $ \_   _   -> return a
  m >>= k   = Bivariate $ \rux ruy ->
                getBivariate m rux ruy >>= \a ->
                (getBivariate . k) a rux ruy   


runBivariate :: Range ux -> Range uy 
             -> ScalingContext ux uy u -> Bivariate ux uy u a -> a
runBivariate rux ruy ctx ma = runScaling ctx (getBivariate ma rux ruy)


liftScaling :: Scaling ux uy u a -> Bivariate ux uy u a
liftScaling mf = Bivariate $ \_ _ -> mf

instance ScalingM (Bivariate ux uy u) where
  type XDim (Bivariate ux uy u) = ux
  type YDim (Bivariate ux uy u) = uy
  scaleX u = liftScaling (scaleX u)
  scaleY u = liftScaling (scaleY u)
  scalePt u v  = liftScaling (scalePt u v)
  scaleVec u v = liftScaling (scaleVec u v)



xrange :: Bivariate ux uy u (Range ux)
xrange = Bivariate $ \rux _ -> return rux

yrange :: Bivariate ux uy u (Range uy) 
yrange = Bivariate $ \_ ruy -> return ruy



rangeCorners :: Bivariate ux uy u (Point2 u, Point2 u)
rangeCorners = Bivariate $ \(Range x0 x1) (Range y0 y1) -> 
                 scalePt x0 y0 >>= \bl ->
                 scalePt x1 y1 >>= \tr ->
                 return (bl,tr)


-- | (w,h) in u
-- 
rangeDimensions :: Num u => Bivariate ux uy u (u,u)
rangeDimensions = 
    (\(P2 x0 y0, P2 x1 y1) -> (x1-x0, y1-y0)) <$> rangeCorners


