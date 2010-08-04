{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
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

    DotAnchor
  , dotCircleEx

  -- * Dots with anchor points
  , dotCircle
  , dotSquare
  , dotChar
  , dotText

  ) where

import Wumpus.Basic.Anchors
import qualified Wumpus.Basic.Dots as BD
import Wumpus.Basic.Monads.Drawing
import Wumpus.Basic.Monads.DrawingCtxClass
import Wumpus.Basic.Monads.TraceClass
import Wumpus.Basic.Utils.Intersection

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

{-
data DotAnchor u = forall s.  
                    DotAnchor { ctr_anchor :: s -> Point2 u
                              , rad_anchor :: s -> Radian -> Point2 u
                              , rep        :: s }

type instance DUnit (DotAnchor u) = u

instance CenterAnchor (DotAnchor u) where
  center (DotAnchor ca _ r) = ca r


instance RadialAnchor (DotAnchor u) where
  radialAnchor theta (DotAnchor _ ra r) = ra r theta
-}

-- An existential thing that supports anchors.
-- This means any dot can retun the same (opaque) structure
--
-- But it does mean that which anchor class are supported is 
-- fixed - the datatype needs a field for each one.
-- Supporting north, southeast etc. will also be tedious...
--
data DotAnchor u = forall s.  
                    DotAnchor { _ctr_anchor :: Point2 u
                              , _rad_anchor :: Radian -> Point2 u }

type instance DUnit (DotAnchor u) = u

instance CenterAnchor (DotAnchor u) where
  center (DotAnchor ca _) = ca


instance RadialAnchor (DotAnchor u) where
   radialAnchor theta (DotAnchor _ ra) = ra theta

circleAnchor :: Floating u => u -> Point2 u -> DotAnchor u
circleAnchor rad ctr = DotAnchor ctr (\theta -> ctr .+^ (avec theta rad))


-- This draws to the trace then returns an opaque thing
-- (a Circle) that supports anchors

dotCircleEx :: ( Monad m, TraceM m (Primitive u), DrawingCtxM m
               , Floating u) 
            => MGraphicF m u (DotAnchor u)
dotCircleEx = \pt -> askDrawingCtx                    >>= \attr -> 
                     markHeight                       >>= \h    ->
                     trace (BD.dotCircle attr pt)     >> 
                     return (circleAnchor (0.5*h) pt)



-- This draws to the trace then returns an opaque thing
-- (a Circle) that supports anchors

dotCircle :: ( Monad m, TraceM m (Primitive u), DrawingCtxM m
             , Fractional u) 
          => MGraphicF m u (Circle u)
dotCircle = \pt -> askDrawingCtx                >>= \attr -> 
                   markHeight                   >>= \h    ->
                   trace (BD.dotCircle attr pt) >> 
                   return (Circle pt (0.5*h))

data Circle u = Circle 
      { _circ_ctr    :: Point2 u 
      , _circ_radius :: u
      }

type instance DUnit (Circle u) = u

instance CenterAnchor (Circle u) where
  center (Circle ctr _) = ctr

instance Floating u => RadialAnchor (Circle u) where
  radialAnchor theta (Circle ctr r) = ctr .+^ (avec theta r)


instance Floating u => CardinalAnchor (Circle u) where
  north (Circle ctr r) = ctr .+^ (avec (pi/2)   r)
  south (Circle ctr r) = ctr .+^ (avec (3*pi/2) r)
  east  (Circle ctr r) = ctr .+^ (avec  0       r)
  west  (Circle ctr r) = ctr .+^ (avec  pi      r)
  


instance Floating u => CardinalAnchor2 (Circle u) where
  northeast (Circle ctr r) = ctr .+^ (avec (pi/4)   r)
  southeast (Circle ctr r) = ctr .+^ (avec (7*pi/4) r)
  southwest (Circle ctr r) = ctr .+^ (avec (5*pi/4) r)
  northwest (Circle ctr r) = ctr .+^ (avec (3*pi/4) r)


data Rect u = Rect 
      { _rect_ctr           :: Point2 u
      , _rect_half_width    :: u
      , _rect_half_height   :: u
      }




type instance DUnit (Rect u) = u

instance CenterAnchor (Rect u) where
  center (Rect ctr _ _) = ctr


instance (Floating u, Real u) => RadialAnchor (Rect u) where
  radialAnchor theta (Rect ctr hw hh) = 
      maybe ctr id $ findIntersect ctr theta $ rectangleLines ctr hw hh


dotSquare :: ( Monad m, TraceM m (Primitive u), DrawingCtxM m
             , Fractional u) 
          => MGraphicF m u (Rect u)
dotSquare = \pt -> askDrawingCtx                >>= \attr -> 
                   markHeight                   >>= \h    ->
                   trace (BD.dotSquare attr pt) >> 
                   return (Rect pt (0.5*h) (0.5*h))


dotChar :: ( Monad m, TraceM m (Primitive u), DrawingCtxM m
           , Fractional u) 
          => Char -> MGraphicF m u (Rect u)
dotChar ch = dotText [ch]

dotText :: ( Monad m, TraceM m (Primitive u), DrawingCtxM m
           , Fractional u) 
          => String -> MGraphicF m u (Rect u)
dotText str = \pt -> askDrawingCtx                  >>= \attr  -> 
                     textDimensions str             >>= \(w,h) ->
                     trace (BD.dotText str attr pt) >>
                     return (Rect pt (0.5*w) (0.5*h))

