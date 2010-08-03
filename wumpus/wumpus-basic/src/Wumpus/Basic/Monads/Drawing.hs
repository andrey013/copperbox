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
    
    node
  , at
  ) where

import Wumpus.Basic.Graphic
import Wumpus.Basic.Monads.TraceClass
import Wumpus.Basic.Monads.TurtleClass

import Wumpus.Core                              -- package: wumpus-core

node :: (TraceM m (Primitive u), TurtleM m u) => GraphicF u -> m ()
node gF = getLoc >>= \pt -> trace (gF pt)

infixr 6 `at` 
at :: (Num u, TraceM m (Primitive u), TurtleM m u) 
   => GraphicF u -> (Int,Int) -> m ()
at gF coord = scaleCoord coord >>= \pt -> trace (gF pt)

