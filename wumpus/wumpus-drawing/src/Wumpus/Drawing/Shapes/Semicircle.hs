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
-- Semicircle - note some Anchor instances are TODO. 
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


runSemicircle :: (u -> u -> u -> ShapeCTM u -> a) -> Semicircle u -> a
runSemicircle fn (Semicircle { sc_ctm       = ctm
                             , sc_radius    = radius
                             , sc_syn_props = syn    }) = 
    fn radius (sc_ctr_minor syn) (sc_ctr_major syn) ctm


instance (Real u, Floating u) => CenterAnchor (Semicircle u) where
  center = ctmCenter . sc_ctm



instance (Real u, Floating u) => CardinalAnchor (Semicircle u) where
  north = runSemicircle $ \_ _    cmaj -> projectPoint $ P2 0  cmaj
  south = runSemicircle $ \_ cmin _    -> projectPoint $ P2 0  (-cmin)
  east  = runSemicircle $ \r cmin _    -> let x = pyth r cmin 
                                          in projectPoint $ P2 x    0
  west  = runSemicircle $ \r cmin _    -> let x = pyth r cmin 
                                          in projectPoint $ P2 (-x) 0

pyth :: Floating u => u -> u -> u
pyth hyp s1 = sqrt $ pow2 hyp - pow2 s1
  where
    pow2 = (^ (2::Int))

-- TODO - Radial and Cardinal2 instances


--------------------------------------------------------------------------------
-- Construction

-- | 'semicircle'  : @ radius -> Shape @
--
semicircle :: (Real u, Floating u) => u -> Shape u (Semicircle u)
semicircle radius = 
    let props = synthesizeProps radius
    in makeShape (mkSemicircle radius props) 
                 (mkSemicirclePath radius (sc_ctr_minor props))
          

synthesizeProps :: Floating u => u -> SyntheticProps u
synthesizeProps radius = 
    SyntheticProps { sc_ctr_minor  = cminor
                   , sc_ctr_major  = cmajor
                   }
  where
    cminor = (4 * radius) / (3 * pi)
    cmajor = radius - cminor


mkSemicircle :: Num u => u -> SyntheticProps u -> LocThetaCF u (Semicircle u)
mkSemicircle radius props = promoteR2 $ \ctr theta -> 
    pure $ Semicircle { sc_ctm = makeShapeCTM ctr theta
                      , sc_radius = radius
                      , sc_syn_props = props 
                      }



mkSemicirclePath :: (Real u, Floating u, Ord u) 
                 => u -> u -> LocThetaCF u (Path u)
mkSemicirclePath radius cminor = promoteR2 $ \(P2 x y) theta ->
    let ctr                 = P2 x (y - cminor)
        (p0, p1,  p2,  p3)  = bezierArc radius 0         (0.25*pi) ctr 
        (_,  p4,  p5,  p6)  = bezierArc radius (0.25*pi) (0.50*pi) ctr 
        (_,  p7,  p8,  p9)  = bezierArc radius (0.50*pi) (0.75*pi) ctr 
        (_,  p10, p11, p12) = bezierArc radius (0.75*pi) pi        ctr 
    in pure $ traceCurvePoints $ map (rotateAbout theta ctr)
                               $ [p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12]



