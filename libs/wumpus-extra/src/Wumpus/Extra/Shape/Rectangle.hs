{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Shape.Rectangle
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Rectangle
-- 
--------------------------------------------------------------------------------

module Wumpus.Extra.Shape.Rectangle
  ( 

    Rectangle(..)
  , rectangle
  , strokeRectangle
  , fillRectangle
  

  ) where

import Wumpus.Core hiding ( CTM )
import Wumpus.Extra.Shape.Base
import Wumpus.Extra.Utils

import Data.AffineSpace


-- | Rectangles.
--
data Rectangle u = Rectangle 
      { rectangle_center        :: Point2 u
      , rectangle_half_width    :: u
      , rectangle_half_height   :: u
      , rectangle_ctm           :: CTM u
      , rectangle_label         :: Maybe ShapeLabel
      }

type instance DUnit (Rectangle u) = u


-- CTM * ctr * half_width * half_height      
--
withGeom :: Num u => (CTM u -> Point2 u -> u -> u -> a) -> Rectangle u -> a
withGeom f rect = f ctm ctr hw hh
  where
    ctm = rectangle_ctm    rect
    ctr = rectangle_center rect
    hw  = rectangle_half_width  rect
    hh  = rectangle_half_height rect 
     

--------------------------------------------------------------------------------
-- Instances 
  

instance (Fractional u) => AnchorCenter (Rectangle u) where
  center = withGeom $ \ ctm ctr _ _ -> ctm *# ctr



instance (Fractional u) =>  AnchorCardinal (Rectangle u) where
  north = withGeom $ \ ctm ctr _  hh -> ctm *# (ctr .+^ vvec hh)
  south = withGeom $ \ ctm ctr _  hh -> ctm *# (ctr .-^ vvec hh)
  east  = withGeom $ \ ctm ctr hw _  -> ctm *# (ctr .+^ hvec hw)
  west  = withGeom $ \ ctm ctr hw _  -> ctm *# (ctr .-^ hvec hw)

  northeast = withGeom $ \ ctm ctr hw hh -> ctm *# (ctr .+^ V2 hw hh)
  southeast = withGeom $ \ ctm ctr hw hh -> ctm *# (ctr .+^ V2 hw (-hh))
  southwest = withGeom $ \ ctm ctr hw hh -> ctm *# (ctr .+^ V2 (-hw) (-hh))
  northwest = withGeom $ \ ctm ctr hw hh -> ctm *# (ctr .+^ V2 (-hw) hh)


-- helper
updateCTM :: (CTM u -> CTM u) -> Rectangle u -> Rectangle u
updateCTM f = pstar (\m s -> s { rectangle_ctm = f m } ) rectangle_ctm


instance (Floating u, Real u) => Rotate (Rectangle u) where
  rotate r = updateCTM (rotateCTM r)

instance (Floating u, Real u) => RotateAbout (Rectangle u) where
  rotateAbout r pt = updateCTM (rotateAboutCTM r pt)

instance Num u => Scale (Rectangle u) where
  scale x y = updateCTM (scaleCTM x y)

instance Num u => Translate (Rectangle u) where
  translate x y = updateCTM (translateCTM x y)


instance AddLabel (Rectangle u) where
  r `addLabel` text = pstar fn rectangle_label r
    where
      fn Nothing    s = s { rectangle_label = Just $ basicLabel text }
      fn (Just lbl) s = s { rectangle_label = Just $ updateText text lbl } 
     

--------------------------------------------------------------------------------
-- Construction

-- | @rectangle : width * height * center_pt -> rectangle@
--
rectangle :: Fractional u => u -> u -> Point2 u -> Rectangle u
rectangle w h ctr = Rectangle ctr (w * 0.5) (h * 0.5) identityMatrix Nothing


--------------------------------------------------------------------------------
-- Drawing 

--


drawRectangle :: (Fractional u, Ord u)   
              => (Path u -> Primitive u) -> Rectangle u -> Composite u
drawRectangle drawF rect = (flip withGeom) rect $ \ctm ctr _ _ -> 
    labelledComposite ctm ctr (rectangle_label rect) shape
  where
    shape = drawF $ vertexPath $ extractVertexList rect


strokeRectangle :: (Fractional u, Ord u, Stroke t) 
                => t -> Rectangle u -> Composite u
strokeRectangle t = drawRectangle (cstroke t)

fillRectangle :: (Fractional u, Ord u, Fill t) 
              => t -> Rectangle u -> Composite u
fillRectangle t = drawRectangle (fill t)


extractVertexList :: Fractional u => Rectangle u -> [Point2 u]
extractVertexList rect = [bl,br,tr,tl]
  where
    bl        = southwest rect
    tr        = northeast rect
    br        = southeast rect
    tl        = northwest rect
