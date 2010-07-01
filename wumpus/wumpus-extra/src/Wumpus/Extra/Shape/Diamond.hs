{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Shape.Diamond
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Diamond
-- 
--------------------------------------------------------------------------------

module Wumpus.Extra.Shape.Diamond
  ( 

    Diamond(..)
  , diamond
  , strokeDiamond
  , fillDiamond
  

  ) where

import Wumpus.Core hiding ( CTM )
import Wumpus.Extra.Shape.Base
import Wumpus.Extra.Utils

import Data.AffineSpace


-- | Diamond.
--
data Diamond u = Diamond 
      { diamond_center        :: Point2 u
      , diamond_half_height   :: u
      , diamond_half_width    :: u
      , diamond_ctm           :: CTM u
      , diamond_label         :: Maybe ShapeLabel
      }

type instance DUnit (Diamond u) = u

-- CTM * ctr * half_width * half_height      

withGeom :: Num u => (CTM u -> Point2 u -> u  -> u -> a) -> Diamond u -> a
withGeom f dia = f ctm ctr hw hh
  where
    ctm = diamond_ctm dia
    ctr = diamond_center dia
    hw  = diamond_half_width dia
    hh  = diamond_half_height dia


--------------------------------------------------------------------------------
-- Instances 
  

instance (Fractional u) => AnchorCenter (Diamond u) where
  center = withGeom $ \ ctm ctr _ _ -> ctm *# ctr



instance (Fractional u) =>  AnchorCardinal (Diamond u) where
  north = withGeom $ \ ctm ctr _  hh -> ctm *# (ctr .+^ vvec hh)
  south = withGeom $ \ ctm ctr _  hh -> ctm *# (ctr .-^ vvec hh)
  east  = withGeom $ \ ctm ctr hw _  -> ctm *# (ctr .+^ hvec hw)
  west  = withGeom $ \ ctm ctr hw _  -> ctm *# (ctr .-^ hvec hw)

  northeast = withGeom $ \ _ctm _ctr _hw _hh -> undefined
  southeast = withGeom $ \ _ctm _ctr _hw _hh -> undefined
  southwest = withGeom $ \ _ctm _ctr _hw _hh -> undefined
  northwest = withGeom $ \ _ctm _ctr _hw _hh -> undefined


-- helper
updateCTM :: (CTM u -> CTM u) -> Diamond u -> Diamond u
updateCTM f = pstar (\m s -> s { diamond_ctm = f m } ) diamond_ctm

instance (Floating u, Real u) => Rotate (Diamond u) where
  rotate r = updateCTM (rotateCTM r) 

instance (Floating u, Real u) => RotateAbout (Diamond u) where
  rotateAbout r pt = updateCTM  (rotateAboutCTM r pt)

instance Num u => Scale (Diamond u) where
  scale x y = updateCTM (scaleCTM x y)

instance Num u => Translate (Diamond u) where
  translate x y = updateCTM (translateCTM x y)


instance AddLabel (Diamond u) where
  r `addLabel` text = pstar updateLabel diamond_label r
    where
      updateLabel Nothing  s = s { diamond_label = Just $ basicLabel text }
      updateLabel (Just lbl) s = s { diamond_label = Just $ updateText text lbl }
     

--------------------------------------------------------------------------------
-- Construction

-- | @rectangle : width * height * center_pt -> rectangle@
--
diamond :: Fractional u => u -> u -> Point2 u -> Diamond u
diamond w h ctr = Diamond ctr (w*0.5) (h*0.5) identityMatrix Nothing


--------------------------------------------------------------------------------
-- Drawing 

--

drawDiamond :: (Fractional u, Ord u)   
              => (Path u -> Primitive u) -> Diamond u -> Composite u
drawDiamond drawF dia = (flip withGeom) dia $ \ctm ctr _ _ -> 
    labelledComposite ctm ctr (diamond_label dia) shape
  where
    shape = drawF $ vertexPath $ extractVertexList dia

strokeDiamond :: (Fractional u, Ord u, Stroke t) 
              => t -> Diamond u -> Composite u
strokeDiamond t  = drawDiamond (cstroke t) 


fillDiamond :: (Fractional u, Ord u, Fill t) 
            => t -> Diamond u -> Composite u
fillDiamond t = drawDiamond (fill t)

 

extractVertexList :: Fractional u => Diamond u -> [Point2 u]
extractVertexList = sequence [south,east,north,west]

