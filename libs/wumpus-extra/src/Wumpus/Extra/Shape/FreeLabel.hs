{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Shape.FreeLabel
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

module Wumpus.Extra.Shape.FreeLabel
  ( 
    FreeLabel(..)
  , freeLabel
  , drawFreeLabel

  ) where

import Wumpus.Core hiding ( CTM )
import Wumpus.Extra.Shape.Base
import Wumpus.Extra.Utils

import Data.AffineSpace

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
      { freelabel_string        :: String
      , freelabel_font_props    :: FontAttr
      , freelabel_center        :: Point2 u
      , freelabel_ctm           :: CTM u
      }

type instance DUnit (FreeLabel u) = u

textDimensions :: Fractional u => FreeLabel u -> (u,u)
textDimensions lbl = (text_width, text_height)
  where
    font_sz     = font_size $ freelabel_font_props lbl
    text_width  = textWidth font_sz (length $ freelabel_string lbl)
    text_height = textHeight font_sz


-- CTM * ctr * half_width * half_height      
withGeom :: Fractional u => (CTM u -> Point2 u -> u -> u -> a) -> FreeLabel u -> a
withGeom f lbl = 
    f (freelabel_ctm lbl) (freelabel_center lbl) (width*0.5) (height*0.5)
  where
    (width,height) = textDimensions lbl

--------------------------------------------------------------------------------
-- instances


-- helper
updateCTM :: (CTM u -> CTM u) -> FreeLabel u -> FreeLabel u
updateCTM f = pstar (\m s -> s { freelabel_ctm = f m } ) freelabel_ctm

instance (Floating u, Real u) => Rotate (FreeLabel u) where
  rotate r = updateCTM (rotateCTM r)

instance (Floating u, Real u) => RotateAbout (FreeLabel u) where
  rotateAbout r pt = updateCTM (rotateAboutCTM r pt)

instance Num u => Scale (FreeLabel u) where
  scale x y = updateCTM (scaleCTM x y)

instance Num u => Translate (FreeLabel u) where
  translate x y = updateCTM (translateCTM x y)



instance (Fractional u) => AnchorCenter (FreeLabel u) where
  center = withGeom $ \ctm ctr _ _ -> ctm *# ctr

instance (Fractional u) =>  AnchorCardinal (FreeLabel u) where
  north = withGeom $ \ctm ctr _  hh -> ctm *# (ctr .+^ vvec hh)
  south = withGeom $ \ctm ctr _  hh -> ctm *# (ctr .-^ vvec hh)
  east  = withGeom $ \ctm ctr hw _  -> ctm *# (ctr .+^ hvec hw)
  west  = withGeom $ \ctm ctr hw _  -> ctm *# (ctr .-^ hvec hw)

  northeast = withGeom $ \ctm ctr hw hh  -> ctm *# (ctr .+^ V2 hw hh)
  southeast = withGeom $ \ctm ctr hw hh  -> ctm *# (ctr .+^ V2 hw (-hh))
  southwest = withGeom $ \ctm ctr hw hh  -> ctm *# (ctr .+^ V2 (-hw) (-hh))
  northwest = withGeom $ \ctm ctr hw hh  -> ctm *# (ctr .+^ V2 (-hw) hh)


freeLabel :: Num u => FontAttr -> String -> Point2 u -> FreeLabel u
freeLabel attr s ctr = FreeLabel s attr ctr identityMatrix



drawFreeLabel :: (Fractional u, PSColour c) 
              => c -> FreeLabel u -> Composite u
drawFreeLabel c lbl = 
    simpleComposite $ transform (freelabel_ctm lbl) $ 
       textlabel (psColour c, font_attrs) label_text bottom_left
  where
    bottom_left = southwest lbl
    font_attrs  = freelabel_font_props lbl
    label_text  = freelabel_string     lbl