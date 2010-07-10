{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Shapes.Rectangle
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

module Wumpus.Shapes.Rectangle
  ( 

    Rectangle(..)
  , rectangle
  , strokeRectangle
  , fillRectangle
  

  ) where

import Wumpus.Shapes.Base


import Wumpus.Core hiding ( CTM )       -- package: wumpus-core
import Wumpus.Basic.Graphic             -- package: wumpus-basic

import Data.AffineSpace                 -- package: vector-space


-- | Rectangles.
--
data Rectangle u = Rectangle 
      { _center        :: Point2 u
      , _half_width    :: u
      , _half_height   :: u
      , _rotation      :: Radian
      , _label         :: Maybe ShapeLabel
      }

type instance DUnit (Rectangle u) = u




-- CTM * ctr * half_width * half_height      
--
withGeom :: Num u => (Point2 u -> Radian -> u -> u -> a) -> Rectangle u -> a
withGeom f rect = f ctr ang hw hh
  where
    ang = _rotation    rect
    ctr = _center      rect
    hw  = _half_width  rect
    hh  = _half_height rect 
     
-- What to call this.... ?
obelisk :: (Real u, Floating u) => (u -> u -> Vec2 u) -> Rectangle u -> Point2 u
obelisk f = withGeom $ \ctr ang hw hh -> ctr .+^ rotateAbout ang ctr (f hw hh) 

--------------------------------------------------------------------------------
-- Instances 
  

instance AnchorCenter (Rectangle u) where
  center (Rectangle { _center = ctr }) = ctr




instance (Real u, Floating u) =>  AnchorCardinal (Rectangle u) where
  north = obelisk $ \ _  hh -> vvec hh
  south = obelisk $ \ _  hh -> vvec (-hh)
  east  = obelisk $ \ hw _  -> hvec hw
  west  = obelisk $ \ hw _  -> hvec (-hw)

  northeast = obelisk $ \ hw hh -> V2 hw hh
  southeast = obelisk $ \ hw hh -> V2 hw (-hh)
  southwest = obelisk $ \ hw hh -> V2 (-hw) (-hh)
  northwest = obelisk $ \ hw hh -> V2 (-hw) hh

{-

instance (Floating u, Real u) => Rotate (Rectangle u) where
  rotate r = updateCTM (rotateCTM r)

instance (Floating u, Real u) => RotateAbout (Rectangle u) where
  rotateAbout r pt = updateCTM (rotateAboutCTM r pt)

instance Num u => Scale (Rectangle u) where
  scale x y = updateCTM (scaleCTM x y)

instance Num u => Translate (Rectangle u) where
  translate x y = updateCTM (translateCTM x y)
-}

instance AddLabel (Rectangle u) where
  r `addLabel` text = star fn _label r
    where
      fn Nothing    s = s { _label = Just $ basicLabel text }
      fn (Just lbl) s = s { _label = Just $ updateText text lbl } 
     

-- starling...

star     :: (a -> r -> ans) 
          -> (r -> a) 
          -> r -> ans
star f fa x = f (fa x) x

--------------------------------------------------------------------------------
-- Construction

-- | @rectangle : width * height * center_pt -> rectangle@
--
rectangle :: Fractional u => u -> u -> Point2 u -> Rectangle u
rectangle w h ctr = Rectangle ctr (w * 0.5) (h * 0.5) 0 Nothing


--------------------------------------------------------------------------------
-- Drawing 

--


drawRectangle :: (Real u, Floating u)   
              => (Path u -> Primitive u) -> Rectangle u -> Graphic u
drawRectangle drawF rect = labelpic . rectpic
  where
    labelpic = maybe id (\lbl -> wrapG $ drawShapeLabel lbl (_center rect)) $ _label rect

    rectpic   = wrapG $ drawF $ vertexPath $ extractVertexList rect

strokeRectangle :: (Real u, Floating u, Stroke t) 
                => t -> Rectangle u -> Graphic u
strokeRectangle t = drawRectangle (cstroke t)

fillRectangle :: (Real u, Floating u, Fill t) 
              => t -> Rectangle u -> Graphic u
fillRectangle t = drawRectangle (fill t)


extractVertexList :: (Real u, Floating u) => Rectangle u -> [Point2 u]
extractVertexList rect = [bl,br,tr,tl]
  where
    bl        = southwest rect
    tr        = northeast rect
    br        = southeast rect
    tl        = northwest rect
