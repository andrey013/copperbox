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

  -- * Existential anchor type
    DotAnchor

  , adotCircle

  -- * Dots with anchor points
  , dotCircle
  , dotDisk
  , dotSquare
  , dotChar
  , dotText

  ) where

import Wumpus.Basic.Anchors
import qualified Wumpus.Basic.Dots                      as BD
import qualified Wumpus.Basic.Graphic.DrawingAttr       as DA
import Wumpus.Basic.Monads.Drawing
import Wumpus.Basic.Monads.DrawingCtxClass
import Wumpus.Basic.Monads.TraceClass
import Wumpus.Basic.Utils.Intersection

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space



-- An existential thing that supports anchors.
-- This means any dot can retun the same (opaque) structure
--
-- But it does mean that which anchor class are supported is 
-- fixed - the datatype needs a field for each one.
-- Supporting north, southeast etc. will also be tedious...
--
data DotAnchor u = forall s.  
                    DotAnchor { center_anchor   :: Point2 u
                              , radial_anchor   :: Radian   -> Point2 u
                              , cardinal_anchor :: Cardinal -> Point2 u }

data Cardinal = NN | NE | EE | SE | SS | SW | WW | NW
  deriving (Eq,Show) 

type instance DUnit (DotAnchor u) = u

instance CenterAnchor (DotAnchor u) where
  center (DotAnchor ca _ _) = ca

instance RadialAnchor (DotAnchor u) where
   radialAnchor theta (DotAnchor _ ra _) = ra theta

instance CardinalAnchor (DotAnchor u) where
   north (DotAnchor _ _ c1) = c1 NN
   south (DotAnchor _ _ c1) = c1 SS
   east  (DotAnchor _ _ c1) = c1 EE
   west  (DotAnchor _ _ c1) = c1 WW



instance CardinalAnchor2 (DotAnchor u) where
   northeast (DotAnchor _ _ c1) = c1 NE
   southeast (DotAnchor _ _ c1) = c1 SE
   southwest (DotAnchor _ _ c1) = c1 SW
   northwest (DotAnchor _ _ c1) = c1 NW


circleAnchor :: Floating u => u -> Point2 u -> DotAnchor u
circleAnchor rad ctr = DotAnchor ctr 
                                 (\theta -> ctr .+^ (avec theta rad))
                                 (radialCardinal rad ctr)




radialCardinal :: Floating u => u -> Point2 u ->  Cardinal -> Point2 u
radialCardinal rad ctr NN = ctr .+^ (avec (pi/2)     rad) 
radialCardinal rad ctr NE = ctr .+^ (avec (pi/4)     rad) 
radialCardinal rad ctr EE = ctr .+^ (avec  0         rad) 
radialCardinal rad ctr SE = ctr .+^ (avec (7/4 * pi) rad) 
radialCardinal rad ctr SS = ctr .+^ (avec (6/4 * pi) rad) 
radialCardinal rad ctr SW = ctr .+^ (avec (5/4 * pi) rad) 
radialCardinal rad ctr WW = ctr .+^ (avec  pi        rad) 
radialCardinal rad ctr NW = ctr .+^ (avec (3/4 * pi) rad) 


-- Rectangle cardinal points are at \"middles and corners\".
--

rectCardinal :: Floating u => u ->  u -> Point2 u -> Cardinal -> Point2 u
rectCardinal _  hh ctr NN = ctr .+^ (vvec hh) 
rectCardinal hw hh ctr NE = ctr .+^ (vec  hw     hh) 
rectCardinal hw _  ctr EE = ctr .+^ (hvec hw) 
rectCardinal hw hh ctr SE = ctr .+^ (vec  hw    (-hh)) 
rectCardinal _  hh ctr SS = ctr .+^ (vvec (-hh)) 
rectCardinal hw hh ctr SW = ctr .+^ (vec  (-hw) (-hh) )
rectCardinal hw _  ctr WW = ctr .+^ (hvec (-hw)) 
rectCardinal hw hh ctr NW = ctr .+^ (vec  (-hw)  hh) 


rectangleAnchor :: (Real u, Floating u) =>  u -> u -> Point2 u -> DotAnchor u
rectangleAnchor hw hh ctr = 
    DotAnchor { center_anchor   = ctr
              , radial_anchor   = fn  
              , cardinal_anchor = rectCardinal hw hh ctr }
  where
    fn theta =  maybe ctr id $ findIntersect ctr theta 
                             $ rectangleLines ctr hw hh



adotCircle :: (Floating u, FromPtSize u) => AGraphic u (DotAnchor u)
adotCircle = AGraphic id (BD.dotDisk) mkF
  where
    mkF attr pt = circleAnchor (0.5* DA.markHeight attr) pt


-- This draws to the trace then returns an opaque thing
-- (a Circle) that supports anchors

dotCircle :: ( Monad m, TraceM m (Primitive u), DrawingCtxM m
             , Floating u, FromPtSize u) 
          => MGraphicF m u (DotAnchor u)
dotCircle = \pt -> askDrawingCtx                    >>= \attr -> 
                   markHeight                       >>= \h    ->
                   trace (BD.dotCircle attr pt)     >> 
                   return (circleAnchor (0.5*h) pt)


dotDisk :: ( Monad m, TraceM m (Primitive u), DrawingCtxM m
           , Floating u, FromPtSize u) 
        => MGraphicF m u (DotAnchor u)
dotDisk = \pt -> askDrawingCtx                    >>= \attr -> 
                 markHeight                       >>= \h    ->
                 trace (BD.dotDisk attr pt)       >> 
                 return (circleAnchor (0.5*h) pt)



dotSquare :: ( Monad m, TraceM m (Primitive u), DrawingCtxM m
             , Real u, Floating u, FromPtSize u) 
          => MGraphicF m u (DotAnchor u)
dotSquare = \pt -> askDrawingCtx                >>= \attr -> 
                   markHeight                   >>= \h    ->
                   trace (BD.dotSquare attr pt) >> 
                   return (rectangleAnchor (0.5*h) (0.5*h) pt)





dotChar :: ( Monad m, TraceM m (Primitive u), DrawingCtxM m
           , Real u, Floating u, FromPtSize u) 
          => Char -> MGraphicF m u (DotAnchor u)
dotChar ch = dotText [ch]

dotText :: ( Monad m, TraceM m (Primitive u), DrawingCtxM m
           , Real u, Floating u, FromPtSize u) 
          => String -> MGraphicF m u (DotAnchor u)
dotText str = \pt -> askDrawingCtx                  >>= \attr  -> 
                     textDimensions str             >>= \(w,h) ->
                     trace (BD.dotText str attr pt) >>
                     return (rectangleAnchor (0.5*w) (0.5*h) pt)


