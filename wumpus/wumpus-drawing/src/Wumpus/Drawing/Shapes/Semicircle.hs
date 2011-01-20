{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Shapes.Semicircle
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Simple shapes - rectangle, circle diamond, ellipse.
--
-- Note - the center calculation is arbitrary. I have picked a 
-- value \"that looks good\" for the ratio. Finding and 
-- implementing a more geometrically correct method is a TODO.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Shapes.Semicircle
  ( 

    Semicircle
  , DSemicircle
  , semicircle

  ) where

import Wumpus.Drawing.Paths
import Wumpus.Drawing.Shapes.Base

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space 

import Control.Applicative



--------------------------------------------------------------------------------
-- Circle

data Semicircle u = Semicircle 
      { sc_ctm          :: ShapeCTM u
      , sc_radius       :: !u 
      , sc_syn_props    :: SyntheticProps u
      }



-- | rect_width is the width of the (greater) enclosing rectangle.
data SyntheticProps u = SyntheticProps
      { sc_ctr_minor  :: u
      , sc_ctr_major  :: u
      }
  
type DSemicircle = Semicircle Double

type instance DUnit (Semicircle u) = u


--------------------------------------------------------------------------------
-- Affine trans

mapCTM :: (ShapeCTM u -> ShapeCTM u) -> Semicircle u -> Semicircle u
mapCTM f = (\s i -> s { sc_ctm = f i }) <*> sc_ctm

instance Num u => Scale (Semicircle u) where
  scale sx sy = mapCTM (scale sx sy)


instance Rotate (Semicircle u) where
  rotate ang = mapCTM (rotate ang)
                  

instance (Real u, Floating u) => RotateAbout (Semicircle u) where
  rotateAbout ang pt = mapCTM (rotateAbout ang pt)


instance Num u => Translate (Semicircle u) where
  translate dx dy = mapCTM (translate dx dy)


--------------------------------------------------------------------------------
-- Anchors

{-
runSemicircle :: (u -> ShapeCTM u -> a) -> Semicircle u -> a
runSemicircle fn (Semicircle { sc_ctm = ctm, sc_radius = radius }) = 
    fn radius ctm
-}

instance (Real u, Floating u) => CenterAnchor (Semicircle u) where
  center = ctmCenter . sc_ctm

{-

instance (Real u, Floating u) => CardinalAnchor (Semicircle u) where
  north = runSemicircle $ \r -> projectPoint $ P2 0    r
  south = runSemicircle $ \r -> projectPoint $ P2 0  (-r)
  east  = runSemicircle $ \r -> projectPoint $ P2 r    0
  west  = runSemicircle $ \r -> projectPoint $ P2 (-r) 0


instance (Real u, Floating u) => CardinalAnchor2 (Semicircle u) where
  northeast = radialAnchor (0.25*pi)
  southeast = radialAnchor (1.75*pi)
  southwest = radialAnchor (1.25*pi)
  northwest = radialAnchor (0.75*pi)


instance (Real u, Floating u) => RadialAnchor (Semicircle u) where
  radialAnchor ang = runSemicircle $ \r -> projectPoint $ zeroPt .+^ avec ang r
-}



--------------------------------------------------------------------------------
-- Constructors

-- | 'semicircle'  : @ radius -> shape @
--
semicircle :: (Real u, Floating u) => u -> LocShape u (Semicircle u)
semicircle radius = 
    let props = synthesizeProps radius
    in intoLocShape (mkSemicircle radius props) 
                    (mkSemicirclePath radius (sc_ctr_minor props))
          

synthesizeProps :: Fractional u => u -> SyntheticProps u
synthesizeProps radius = 
    SyntheticProps { sc_ctr_minor  = cminor
                   , sc_ctr_major  = cmajor
                   }
  where
    cminor = radius * 0.4
    cmajor = radius - cminor


mkSemicircle :: Num u => u -> SyntheticProps u -> LocCF u (Semicircle u)
mkSemicircle radius props = promoteR1 $ \ctr -> 
    pure $ Semicircle { sc_ctm = makeShapeCTM ctr
                      , sc_radius = radius
                      , sc_syn_props = props 
                      }



mkSemicirclePath :: (Floating u, Ord u) => u -> u -> LocCF u (Path u)
mkSemicirclePath radius cminor = promoteR1 $ \(P2 x y) ->
    let ctr                 = P2 x (y - cminor)
        (p0, p1,  p2,  p3)  = bezierArc radius 0         (0.25*pi) ctr 
        (_,  p4,  p5,  p6)  = bezierArc radius (0.25*pi) (0.50*pi) ctr 
        (_,  p7,  p8,  p9)  = bezierArc radius (0.50*pi) (0.75*pi) ctr 
        (_,  p10, p11, p12) = bezierArc radius (0.75*pi) pi        ctr 
    in pure $ traceCurvePoints $ [p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12]



