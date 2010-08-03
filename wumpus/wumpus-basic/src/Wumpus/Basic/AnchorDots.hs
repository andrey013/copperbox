{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.AnchorDots
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies, GADTs and more
--
-- Dots with anchors.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.AnchorDots
  ( 


  -- * Dots with anchor points
    dotCircle

  ) where

import Wumpus.Basic.Anchors
import qualified Wumpus.Basic.Dots as BD
import Wumpus.Basic.Monads.Drawing
import Wumpus.Basic.Monads.DrawingCtxClass
import Wumpus.Basic.Monads.TraceClass

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                 -- package: vector-space




-- This draws to the trace then returns an opaque thing
-- (a Circle) that supports anchors

dotCircle :: ( Monad m, TraceM m (Primitive u), DrawingCtxM m
             , Fractional u) 
          => MGraphicF m u (Circle u)
dotCircle = \pt -> askDrawingCtx                >>= \attr -> 
                   markHeight                   >>= \h    ->
                   trace (BD.dotCircle attr pt) >> 
                   return (Circle (0.5*h) pt)

data Circle u = Circle 
      { _circ_radius :: u
      , _circ_ctr    :: Point2 u 
      }

type instance DUnit (Circle u) = u

instance CenterAnchor (Circle u) where
  center (Circle _ ctr) = ctr

instance Floating u => RadialAnchor (Circle u) where
  radialAnchor theta (Circle r ctr) = ctr .+^ (avec theta r)


instance Floating u => CardinalAnchor (Circle u) where
  north (Circle r ctr) = ctr .+^ (avec (pi/2)   r)
  south (Circle r ctr) = ctr .+^ (avec (3*pi/2) r)
  east  (Circle r ctr) = ctr .+^ (avec  0       r)
  west  (Circle r ctr) = ctr .+^ (avec  pi      r)
  