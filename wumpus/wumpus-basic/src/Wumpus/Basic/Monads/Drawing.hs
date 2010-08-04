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
    
    MGraphicF 
  , traceG
  , node
  , at
  ) where

import Wumpus.Basic.Graphic
import Wumpus.Basic.Monads.TraceClass
import Wumpus.Basic.Monads.TurtleClass

import Wumpus.Core                              -- package: wumpus-core


type MGraphicF m u a = Point2 u -> m a

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

