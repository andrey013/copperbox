{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Monads.Drawing
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Drawing operations
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Monads.Drawing
  (
    
    AGraphic(..)
  , node
  , at

  -- OLD 
  , MGraphicF 

  ) where

import Wumpus.Basic.Graphic
import Wumpus.Basic.Graphic.DrawingAttr
import Wumpus.Basic.Monads.DrawingCtxClass
import Wumpus.Basic.Monads.TraceClass
import Wumpus.Basic.Monads.TurtleClass

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative


data AGraphic u a = AGraphic 
       { agAttrF    :: DrawingAttr -> DrawingAttr
       , agDrawF    :: DrawingAttr -> Point2 u -> Graphic u
       , agMakeF    :: DrawingAttr -> Point2 u -> a
       }

instance Functor (AGraphic u) where
  fmap f (AGraphic af df mf) = AGraphic af df (\attr pt -> f $ mf attr pt)


-- This doesn't work like at on MGraphicF, as the point is not 
-- scaled w.r.t. TurtleScaleM ...
--
at :: AGraphic u a -> Point2 u -> AGraphic u a
at (AGraphic af df mf) pt = AGraphic af (\attr _ -> df attr pt)
                                          (\attr _ -> mf attr pt)

instance Applicative (AGraphic u) where
  pure a = AGraphic id (\_ _ -> id) (\_ _ -> a)
  (AGraphic af1 df1 mf1) <*> (AGraphic af2 df2 mf2) = AGraphic af df mf
     where
       af           = af2 . af1
       df attr pt   = df2 (af2 attr) pt . df1 (af1 attr) pt
       mf attr pt   = mf1 attr pt $ mf2 attr pt


-- getPos should be a class method outside of Turtle
-- those Bivariate context from PSC could implement it...

node :: (Num u, TraceM m (Primitive u), DrawingCtxM m, TurtleScaleM m u) 
       => AGraphic u a -> m a
node (AGraphic af df mf) = 
    askDrawingCtx >>= \a0 ->
    getPos        >>= \pt ->
    let attr = af a0 in trace (df attr pt) >> return (mf attr pt)



-- OLD...



type MGraphicF m u a = Point2 u -> m a


{-
traceG :: (Monad m, TraceM m (Primitive u)) => GraphicF u -> MGraphicF m u ()
traceG fn = \pt -> trace (fn pt)

-- MGraphic functions will have to trace themselves...

node :: (TraceM m (Primitive u), TurtleScaleM m u, Num u) 
     => MGraphicF m u a -> m a 
node mgF = getPos >>= \pt -> mgF pt




infixr 6 `at` 

at :: (Num u, TraceM m (Primitive u), TurtleScaleM m u) 
   => MGraphicF m u a -> (Int,Int) -> m a
at mgF coord = scaleCoord coord >>= \pt -> mgF pt

-}