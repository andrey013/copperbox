{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Shapes.Trapezium
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Trapezium.
--
-- Note cardinal anchors correspond directly to the compass 
-- positions.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Shapes.Trapezium
  ( 

    Trapezium
  , DTrapezium
  , trapezium
  , ztrapezium


  ) where

import Wumpus.Drawing.Paths
import Wumpus.Drawing.Shapes.Base

import Wumpus.Basic.Geometry.Intersection       -- package: wumpus-basic
import Wumpus.Basic.Geometry.Paths
import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core


import Data.VectorSpace                         -- package: vector-space

import Control.Applicative




--------------------------------------------------------------------------------
-- Trapezium

-- | A trapezium.
--
data Trapezium u = Trapezium 
      { tz_ctm          :: ShapeCTM u
      , tz_base_width   :: !u
      , tz_height       :: !u
      , tz_base_l_ang   :: Radian
      , tz_base_r_ang   :: Radian
      }


type DTrapezium = Trapezium Double

instance Functor Trapezium where
  fmap f (Trapezium ctm bw h lang rang) = 
    Trapezium (fmap f ctm) (f bw) (f h) lang rang

--------------------------------------------------------------------------------
-- Affine trans

mapCTM :: (ShapeCTM u -> ShapeCTM u) -> Trapezium u -> Trapezium u
mapCTM f = (\s i -> s { tz_ctm = f i }) <*> tz_ctm


instance CtxRotate Trapezium u where
  ctxRotate sz ang = mapCTM (ctxRotate sz ang)
              
instance InterpretUnit u => CtxRotateAbout Trapezium u where
  ctxRotateAbout sz ang pt = mapCTM (ctxRotateAbout sz ang pt)

instance InterpretUnit u => CtxScale Trapezium u where
  ctxScale sz sx sy = mapCTM (ctxScale sz sx sy)

instance InterpretUnit u => CtxTranslate Trapezium u where
  ctxTranslate sz dx dy = mapCTM (ctxTranslate sz dx dy)


--------------------------------------------------------------------------------
-- Anchors

-- | 'runDisplaceCenter' : @ ( half_base_width 
--                           * half_height
--                           * left_base_ang 
--                           * right_base_ang -> Vec ) * trapzium -> Point @
--
runDisplaceCenter :: (Real u, Floating u, InterpretUnit u)
                  => (u -> u -> Radian -> Radian -> Vec2 u) 
                  -> Trapezium u -> Anchor u
runDisplaceCenter fn (Trapezium { tz_ctm          = ctm
                                , tz_base_width   = bw
                                , tz_height       = h
                                , tz_base_l_ang   = lang
                                , tz_base_r_ang   = rang }) =
    projectFromCtr (fn (0.5 * bw) (0.5 * h) lang rang) ctm

instance (Real u, Floating u, InterpretUnit u) => 
    CenterAnchor Trapezium u where
  center = runDisplaceCenter $ \_ _ _ _ -> V2 0 0



instance (Real u, Floating u, InterpretUnit u) => 
    BottomCornerAnchor Trapezium u where
  bottomLeftCorner  = runDisplaceCenter $ \hbw hh _ _  -> V2 (-hbw) (-hh)
  bottomRightCorner = runDisplaceCenter $ \hbw hh _ _  -> V2  hbw   (-hh)


instance (Real u, Floating u, InterpretUnit u) => 
    TopCornerAnchor Trapezium u where
  topLeftCorner  = runDisplaceCenter $ \hbw hh lang _  -> 
                     let vbase = V2 (-hbw) (-hh)
                         vup   = leftSideVec (2*hh) lang 
                     in vbase ^+^ vup
  topRightCorner = runDisplaceCenter $ \hbw hh _ rang  ->
                     let vbase = V2  hbw   (-hh)
                         vup   = rightSideVec (2*hh) rang
                     in vbase ^+^ vup


instance (Real u, Floating u, InterpretUnit u, LengthTolerance u) => 
    SideMidpointAnchor Trapezium u where
  sideMidpoint n a = step (n `mod` 4) 
    where
      step 1 = north a
      step 2 = west a
      step 3 = south a
      step _ = east a



instance (Real u, Floating u, InterpretUnit u, LengthTolerance u) => 
    CardinalAnchor Trapezium u where
  north = runDisplaceCenter $ \_ hh _ _ -> V2 0 hh
  south = runDisplaceCenter $ \_ hh _ _ -> V2 0 (-hh)
  east  = tzRadialAnchor 0
  west  = tzRadialAnchor pi


instance (Real u, Floating u, InterpretUnit u, LengthTolerance u) => 
    CardinalAnchor2 Trapezium u where
  northeast = tzRadialAnchor (0.25*pi)
  southeast = tzRadialAnchor (1.75*pi)
  southwest = tzRadialAnchor (1.25*pi)
  northwest = tzRadialAnchor (0.75*pi)



instance (Real u, Floating u, InterpretUnit u, LengthTolerance u) => 
    RadialAnchor Trapezium u where
  radialAnchor = tzRadialAnchor

-- TODO - update this to a quadrant function...
--
tzRadialAnchor :: (Real u, Floating u, InterpretUnit u, LengthTolerance u) 
               => Radian -> Trapezium u -> Query (Point2 u)
tzRadialAnchor theta (Trapezium { tz_ctm        = ctm
                                , tz_base_width = bw
                                , tz_height     = h
                                , tz_base_l_ang = lang
                                , tz_base_r_ang = rang }) =
    post $ findIntersect zeroPt theta $ polygonLineSegments ps
  where 
    ps   = tzPoints bw h lang rang
    post = \ans -> case ans of 
                    Nothing       -> projectFromCtr (V2 0 0) ctm
                    Just (P2 x y) -> projectFromCtr (V2 x y) ctm
    
    
--------------------------------------------------------------------------------
-- Construction


-- | 'trapezium'  : @ base_width * height * bottom_left_ang * 
--     bottom_right_ang -> Shape @
--
--
trapezium :: (Real u, Floating u, InterpretUnit u, LengthTolerance u) 
          => u -> u -> Radian -> Radian -> Shape Trapezium u
trapezium bw h lang rang = 
    makeShape (mkTrapezium bw h lang rang) (mkTrapeziumPath bw h lang rang)


-- | 'ztrapezium'  : @ base_width * height -> Trapezium @
--
--
ztrapezium :: (Real u, Floating u, InterpretUnit u, LengthTolerance u) 
           => u -> u -> Shape Trapezium u
ztrapezium bw h = trapezium bw h ang ang
  where
    ang = d2r (60::Double)


--------------------------------------------------------------------------------


mkTrapezium :: (Real u, Fractional u, InterpretUnit u) 
            => u -> u -> Radian -> Radian -> LocThetaQuery u (Trapezium u)
mkTrapezium bw h lang rang = promoteR2 $ \ctr theta -> 
    pure $ Trapezium { tz_ctm           = makeShapeCTM ctr theta
                     , tz_base_width    = bw
                     , tz_height        = h
                     , tz_base_l_ang    = lang
                     , tz_base_r_ang    = rang
                     }


mkTrapeziumPath :: (Real u, Floating u, InterpretUnit u, LengthTolerance u) 
                => u -> u -> Radian -> Radian -> LocThetaQuery u (Path u)
mkTrapeziumPath bw h lang rang = promoteR2 $ \ctr theta -> 
    pointSize >>= \sz -> 
    let xs = tzPath bw h lang rang ctr 
    in roundCornerShapePath $ map (ctxRotateAbout sz theta ctr) xs


tzPath :: (Real u, Floating u) 
       => u -> u -> Radian -> Radian -> LocCoordPath u
tzPath bw h lang rang (P2 x y) = [ bl, br, tr, tl ]
  where
    half_base = 0.5 * bw
    hh        = 0.5 * h
    br        = P2 (x + half_base ) (y - hh)
    bl        = P2 (x - half_base ) (y - hh)
    tr        = displaceVec (rightSideVec h rang) br
    tl        = displaceVec (leftSideVec h lang) bl


tzPoints :: (Real u, Floating u) 
               => u -> u -> Radian -> Radian -> [Point2 u]
tzPoints bw h lang rang = [ bl, br, tr, tl ]
  where
    half_base = 0.5 * bw
    hh        = 0.5 * h
    bl        = P2 (-half_base) (-hh)
    br        = P2 half_base    (-hh)
    tr        = displaceVec (rightSideVec h rang) br
    tl        = displaceVec (leftSideVec h lang) bl



-- | Calculate the vector that produces the upper-left point given
-- the lower-left point.
--
-- Note - expects ang value 0 < ang < 180, though does not check...
-- 
leftSideVec :: Floating u => u -> Radian -> Vec2 u
leftSideVec h lang | lang <  0.5*pi = less_ninety
                   | lang == 0.5*pi = vvec h
                   | otherwise      = grtr_ninety
  where
    less_ninety = let dist = h / (fromRadian $ sin lang) in avec lang dist
    grtr_ninety = let theta = lang - (0.5*pi) 
                      dist  = h / (fromRadian $ cos theta) 
                  in avec lang dist




-- | Calculate the vector that produces the upper-right point given
-- the lower-right point.
--
-- Note - expects ang value 0 < ang < 180, though does not check...
-- 
rightSideVec :: Floating u => u -> Radian -> Vec2 u
rightSideVec h rang | rang <  0.5*pi = less_ninety
                    | rang == 0.5*pi = vvec h
                    | otherwise      = grtr_ninety
  where
    less_ninety = let dist  = h / (fromRadian $ sin rang) in avec (pi - rang) dist
    grtr_ninety = let theta = rang - (0.5*pi) 
                      dist  = h / (fromRadian $ cos theta) 
                  in avec (pi - rang) dist

