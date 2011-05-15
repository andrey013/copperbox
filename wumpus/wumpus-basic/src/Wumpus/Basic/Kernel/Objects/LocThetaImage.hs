{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.LocThetaImage
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- LocThetaImage and LocThetaGraphic types - these are functional 
-- types from the DrawingContext, start point and angle of 
-- inclination to a graphic /primitive/.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.LocThetaImage
   (
     LocThetaGraphic
   , LocThetaImage

   , DLocThetaGraphic
   , DLocThetaImage

   , uconvLocThetaImageF
   , uconvLocThetaImageZ

   , emptyLocThetaImage
   
   )

   where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.QueryDC
import Wumpus.Basic.Kernel.Objects.Basis
-- import Wumpus.Basic.Kernel.Objects.LocImage

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative
import Data.Monoid

-- | 'LocThetaImage' - function from start point, inclination and
-- DrawingContext to a polymorphic /answer/ and a graphic 
-- /primitive/ (PrimW).
--
newtype LocThetaImage u a = LocThetaImage { 
          getLocThetaImage :: Point2 u -> Radian -> Image u a }

type instance DUnit (LocThetaImage u a) = u
type instance Answer (LocThetaImage u a) = a

type LocThetaGraphic u = LocThetaImage u (UNil u)


-- | Type specialized version of 'LocThetaImage'.
--
type DLocThetaImage a        = LocThetaImage Double a

-- | Type specialized version of 'LocThetaGraphic'.
--
type DLocThetaGraphic        = LocThetaGraphic Double 



instance Functor (LocThetaImage u) where
  fmap f ma = LocThetaImage $ \pt ang -> 
                fmap f $ getLocThetaImage ma pt ang

instance Applicative (LocThetaImage u) where
  pure a    = LocThetaImage $ \_  _   -> pure a
  mf <*> ma = LocThetaImage $ \pt ang -> 
                getLocThetaImage mf pt ang <*> getLocThetaImage ma pt ang


instance Monad (LocThetaImage u) where
  return a  = LocThetaImage $ \_  _   -> return a
  ma >>= k  = LocThetaImage $ \pt ang -> 
                getLocThetaImage ma pt ang >>= \ans -> 
                getLocThetaImage (k ans) pt ang


instance Monoid a => Monoid (LocThetaImage u a) where
  mempty          = pure mempty
  ma `mappend` mb = LocThetaImage $ \pt ang -> 
                      getLocThetaImage ma pt ang 
                         `mappend` getLocThetaImage mb pt ang



instance DrawingCtxM (LocThetaImage u) where
  askDC           = LocThetaImage $ \_  _   -> askDC
  asksDC fn       = LocThetaImage $ \_  _   -> asksDC fn
  localize upd ma = LocThetaImage $ \pt ang -> 
                      localize upd (getLocThetaImage ma pt ang)


instance BinaryObj (LocThetaImage u a) where
  type BinaryR1 (LocThetaImage u a) = Point2 u 
  type BinaryR2 (LocThetaImage u a) = Radian
  
  promoteB fn       = LocThetaImage $ \pt ang -> fn pt ang
  applyB mf pt ang  = getLocThetaImage mf pt ang


-- | Use this to convert 'LocThetaThetaGraphic' or 'LocThetaThetaImage' 
-- with Functor answer.
--
uconvLocThetaImageF :: (InterpretUnit u, InterpretUnit u1, Functor t) 
                    => LocThetaImage u (t u) -> LocThetaImage u1 (t u1)
uconvLocThetaImageF ma = LocThetaImage $ \pt ang -> 
    getFontSize >>= \sz -> 
    let ptu = uconvertF sz pt
    in uconvImageF $ getLocThetaImage ma ptu ang




-- | Use this to convert 'LocThetaImage' with unit-less answer.
--
uconvLocThetaImageZ :: (InterpretUnit u, InterpretUnit u1) 
                    => LocThetaImage u a -> LocThetaImage u1 a
uconvLocThetaImageZ ma = LocThetaImage $ \pt ang -> 
    getFontSize >>= \sz -> 
    let ptu = uconvertF sz pt
    in uconvImageZ $ getLocThetaImage ma ptu ang



-- | Having /empty/ at the specific 'LocThetaImage' type is useful.
-- 
emptyLocThetaImage :: Monoid a => LocThetaImage u a
emptyLocThetaImage = mempty

