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
  , drawFreeLabel

  ) where


import Wumpus.Shapes.Base
import Wumpus.Shapes.Utils

import Wumpus.Core hiding ( CTM )       -- package: wumpus-core
import Wumpus.Basic.Graphic             -- package: wumpus-basic


--------------------------------------------------------------------------------
-- Free floating label


data FreeLabel u = FreeLabel
      { flbl_label         :: ShapeLabel
      , flbl_ctm           :: CTM u
      }

type DFreeLabel = FreeLabel Double


type instance DUnit (FreeLabel u) = u



-- CTM * half_width * half_height      
--
withGeom :: Fractional u => (CTM u -> u -> u -> a) -> FreeLabel u -> a
withGeom f (FreeLabel { flbl_ctm=ctm, flbl_label=lbl }) = 
    f ctm (0.5*twidth) (0.5*theight)
  where
    ((_,attr),text)   = deconsLabel lbl
    font_sz           = font_size attr
    twidth            = textWidth  font_sz (length text)
    theight           = textHeight font_sz
    
     
calcPoint :: (Real u, Floating u) => (u -> u -> Vec2 u) -> FreeLabel u -> Point2 u
calcPoint f = withGeom $ \ctm hw hh -> 
    let (V2 x y) = f hw hh in ctmDisplace x y ctm




--------------------------------------------------------------------------------
-- instances


-- helper
updateCTM :: (CTM u -> CTM u) -> FreeLabel u -> FreeLabel u
updateCTM f = star (\s m -> s { flbl_ctm = f m } ) flbl_ctm

instance (Floating u, Real u) => Rotate (FreeLabel u) where
  rotate r = updateCTM (rotateCTM r)

-- cannnot scale a FreeLabel

instance Num u => Translate (FreeLabel u) where
  translate x y = updateCTM (translateCTM x y)



instance (Real u, Floating u) => AnchorCenter (FreeLabel u) where
    center = ctmCenter . flbl_ctm


instance (Real u, Floating u) =>  AnchorCardinal (FreeLabel u) where
  north = calcPoint $ \ _  hh -> vvec hh
  south = calcPoint $ \ _  hh -> vvec (-hh)
  east  = calcPoint $ \ hw _  -> hvec hw
  west  = calcPoint $ \ hw _  -> hvec (-hw)

  northeast = calcPoint $ \ hw hh -> V2 hw hh
  southeast = calcPoint $ \ hw hh -> V2 hw (-hh)
  southwest = calcPoint $ \ hw hh -> V2 (-hw) (-hh)
  northwest = calcPoint $ \ hw hh -> V2 (-hw) hh


freeLabel :: Num u => FontAttr -> DRGB -> String -> FreeLabel u
freeLabel attr rgb s = FreeLabel (ShapeLabel s attr rgb) identityCTM



drawFreeLabel :: (Real u, Floating u) => FreeLabel u -> Graphic u
drawFreeLabel lbl = labelGraphic (flbl_ctm lbl) $ flbl_label lbl
