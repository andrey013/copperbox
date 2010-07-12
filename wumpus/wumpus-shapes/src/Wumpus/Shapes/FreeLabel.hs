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
  , freeLabel
  , drawFreeLabel

  ) where


import Wumpus.Shapes.Base
import Wumpus.Shapes.Utils

import Wumpus.Core hiding ( CTM )       -- package: wumpus-core
import Wumpus.Basic.Graphic             -- package: wumpus-basic

import Data.AffineSpace                 -- package: vector-space

--------------------------------------------------------------------------------
-- Free floating label

-- Note - a FreeLabel needs some \"drawing attributes\" - Font 
-- style and size.
-- 
-- Unfortunately this /divides/ the TextLabel type class from 
-- wumpus-core into two halves - needs size and font, but wants
-- colour later (only when drawn).
--

data FreeLabel u = FreeLabel
      { flbl_label         :: ShapeLabel
      , flbl_ctm           :: CTM u
      }

type instance DUnit (FreeLabel u) = u


{-
-- CTM * ctr * half_width * half_height      
withGeom :: Fractional u => (CTM u -> Point2 u -> u -> u -> a) -> FreeLabel u -> a
withGeom f lbl = 
    f (freelabel_ctm lbl) (freelabel_center lbl) (width*0.5) (height*0.5)
  where
    (width,height) = textDimensions lbl
-}

--------------------------------------------------------------------------------
-- instances


-- helper
updateCTM :: (CTM u -> CTM u) -> FreeLabel u -> FreeLabel u
updateCTM f = star (\s m -> s { flbl_ctm = f m } ) flbl_ctm

instance (Floating u, Real u) => Rotate (FreeLabel u) where
  rotate r = updateCTM (rotateCTM r)

instance Num u => Scale (FreeLabel u) where
  scale x y = updateCTM (scaleCTM x y)

instance Num u => Translate (FreeLabel u) where
  translate x y = updateCTM (translateCTM x y)



instance (Real u, Floating u) => AnchorCenter (FreeLabel u) where
    center = ctmCenter . flbl_ctm

{-
instance (Fractional u) =>  AnchorCardinal (FreeLabel u) where
  north = withGeom $ \ctm ctr _  hh -> ctm *# (ctr .+^ vvec hh)
  south = withGeom $ \ctm ctr _  hh -> ctm *# (ctr .-^ vvec hh)
  east  = withGeom $ \ctm ctr hw _  -> ctm *# (ctr .+^ hvec hw)
  west  = withGeom $ \ctm ctr hw _  -> ctm *# (ctr .-^ hvec hw)

  northeast = withGeom $ \ctm ctr hw hh  -> ctm *# (ctr .+^ V2 hw hh)
  southeast = withGeom $ \ctm ctr hw hh  -> ctm *# (ctr .+^ V2 hw (-hh))
  southwest = withGeom $ \ctm ctr hw hh  -> ctm *# (ctr .+^ V2 (-hw) (-hh))
  northwest = withGeom $ \ctm ctr hw hh  -> ctm *# (ctr .+^ V2 (-hw) hh)
-}

freeLabel :: Num u => FontAttr -> DRGB -> String -> FreeLabel u
freeLabel attr rgb s = FreeLabel (ShapeLabel s attr rgb) identityCTM



drawFreeLabel :: (Real u, Floating u) => FreeLabel u -> Graphic u
drawFreeLabel lbl = labelGraphic (flbl_ctm lbl) $ flbl_label lbl
