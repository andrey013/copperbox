{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Shapes.FreeLabel
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Coordinate points
-- 
--------------------------------------------------------------------------------

module Wumpus.Shapes.FreeLabel
  ( 
    FreeLabel(..)
  , DFreeLabel
  , freeLabel

  ) where


import Wumpus.Shapes.Base

import Wumpus.Core                              -- package: wumpus-core

import Wumpus.Basic.Anchors                     -- package: wumpus-basic
import Wumpus.Basic.Graphic

import Control.Applicative

--------------------------------------------------------------------------------
-- Free floating label


data FreeLabel u = FreeLabel
      { lbl_half_width    :: u
      , lbl_half_height   :: u
      , lbl_label         :: ShapeLabel
      , lbl_ctm           :: CTM u
      }

type DFreeLabel = FreeLabel Double


type instance DUnit (FreeLabel u) = u


-- FreeLabel is an exception.
--
-- It needs DrawingAttr to calculate geometries (i.e. anchor
-- points). But DrawingAttr are supplied at lifting to AGraphic 
-- time, not at construction time.
--
-- Don\'t know how to handle this at the moment...
-- 
-- Are all anchors intially the center, until the FreeLabel is 
-- lifted to an AGraphic?
--

-- CTM * half_width * half_height      
--
withGeom :: Fractional u => (CTM u -> u -> u -> a) -> FreeLabel u -> a
withGeom f (FreeLabel { lbl_ctm=ctm, lbl_half_width=hw, lbl_half_height=hh }) = 
    f ctm hw hh

{-
  where
    ((_,attr),text)   = deconsLabel lbl
    font_sz           = font_size attr
    twidth            = textWidth  font_sz (length text)
    theight           = textHeight font_sz
-}    
     
calcPoint :: (Real u, Floating u) => (u -> u -> Vec2 u) -> FreeLabel u -> Point2 u
calcPoint f = withGeom $ \ctm hw hh -> 
    let (V2 x y) = f hw hh in ctmDisplace x y ctm

updateCTM :: (CTM u -> CTM u) -> FreeLabel u -> FreeLabel u
updateCTM f = (\s m -> s { lbl_ctm = f m } ) <*> lbl_ctm



--------------------------------------------------------------------------------
-- instances



instance (Floating u, Real u) => Rotate (FreeLabel u) where
  rotate r = updateCTM (rotateCTM r)

-- cannnot scale a FreeLabel

instance Num u => Translate (FreeLabel u) where
  translate x y = updateCTM (translateCTM x y)



instance (Real u, Floating u) => CenterAnchor (FreeLabel u) where
    center = ctmCenter . lbl_ctm


instance (Real u, Floating u) => CardinalAnchor (FreeLabel u) where
  north = calcPoint $ \ _  hh -> vvec hh
  south = calcPoint $ \ _  hh -> vvec (-hh)
  east  = calcPoint $ \ hw _  -> hvec hw
  west  = calcPoint $ \ hw _  -> hvec (-hw)

instance (Real u, Floating u) => CardinalAnchor2 (FreeLabel u) where
  northeast = calcPoint $ \ hw hh -> V2 hw hh
  southeast = calcPoint $ \ hw hh -> V2 hw (-hh)
  southwest = calcPoint $ \ hw hh -> V2 (-hw) (-hh)
  northwest = calcPoint $ \ hw hh -> V2 (-hw) hh



--------------------------------------------------------------------------------

freeLabel :: Num u => String -> FreeLabel u
freeLabel s = FreeLabel { lbl_half_width    = 0
                        , lbl_half_height   = 0
                        , lbl_label         = ShapeLabel s
                        , lbl_ctm           = identityCTM
                        }


textC :: (Real u, Floating u, FromPtSize u) 
      => FreeLabel u -> Graphic u
textC lbl = labelGraphic (lbl_label lbl) (lbl_ctm lbl)



instance (Real u, Floating u, FromPtSize u) => DrawShape (FreeLabel u) where
  drawShape lbl = intoImage (pureDF lbl) (textC lbl)


