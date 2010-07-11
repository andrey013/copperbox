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
  , DRectangle

  , rectangle
  , strokeRectangle
  , fillRectangle
  

  ) where

import Wumpus.Shapes.Base
import Wumpus.Shapes.Utils

import Wumpus.Core hiding ( CTM )       -- package: wumpus-core
import Wumpus.Basic.Graphic             -- package: wumpus-basic

import Data.AffineSpace                 -- package: vector-space


-- | Rectangles.
--
data Rectangle u = Rectangle 
      { rect_center        :: Point2 u
      , rect_half_width    :: u
      , rect_half_height   :: u
      , rect_rotation      :: Radian
      , rect_label         :: Maybe ShapeLabel
      }

type DRectangle = Rectangle Double

type instance DUnit (Rectangle u) = u




-- CTM * ctr * half_width * half_height      
--
withGeom :: Num u => (Point2 u -> Radian -> u -> u -> a) -> Rectangle u -> a
withGeom f (Rectangle ctr hw hh ang _) = f ctr ang hw hh
     
-- What to call this.... ?
obelisk :: (Real u, Floating u) => (u -> u -> Vec2 u) -> Rectangle u -> Point2 u
obelisk f = withGeom $ \ctr ang hw hh -> ctr .+^ rotateAbout ang ctr (f hw hh) 

--------------------------------------------------------------------------------
-- Instances 
  

instance AnchorCenter (Rectangle u) where
  center (Rectangle { rect_center = ctr }) = ctr




instance (Real u, Floating u) =>  AnchorCardinal (Rectangle u) where
  north = obelisk $ \ _  hh -> vvec hh
  south = obelisk $ \ _  hh -> vvec (-hh)
  east  = obelisk $ \ hw _  -> hvec hw
  west  = obelisk $ \ hw _  -> hvec (-hw)

  northeast = obelisk $ \ hw hh -> V2 hw hh
  southeast = obelisk $ \ hw hh -> V2 hw (-hh)
  southwest = obelisk $ \ hw hh -> V2 (-hw) (-hh)
  northwest = obelisk $ \ hw hh -> V2 (-hw) hh



instance (Floating u, Real u) => Rotate (Rectangle u) where
  rotate r = star (\ang s -> s { rect_rotation = circularModulo $ r+ang })
                  rect_rotation


instance Num u => Scale (Rectangle u) where
  scale x y = star2 (\hw hh s -> s { rect_half_width = x*hw,
                                     rect_half_height = y*hh})
                    rect_half_width
                    rect_half_height

instance Num u => Translate (Rectangle u) where
  translate x y = star (\ctr s -> s { rect_center = translate x y ctr} )
                       rect_center 


instance AddLabel (Rectangle u) where
  r `addLabel` text = star fn rect_label r
    where
      fn Nothing    s = s { rect_label = Just $ basicLabel text }
      fn (Just lbl) s = s { rect_label = Just $ updateText text lbl } 
     



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
    labelpic = maybe id (\lbl -> wrapG $ 
                           drawShapeLabel lbl (rect_center rect) (rect_rotation rect))
                        (rect_label rect)

    rectpic   = wrapG $ drawF $ vertexPath $ extractVertexList rect

strokeRectangle :: (Real u, Floating u, Stroke t) 
                => t -> Rectangle u -> Graphic u
strokeRectangle t = drawRectangle (cstroke t)

fillRectangle :: (Real u, Floating u, Fill t) 
              => t -> Rectangle u -> Graphic u
fillRectangle t = drawRectangle (fill t)


instance DrawShape Rectangle where
  strokeShape t = drawRectangle (cstroke t) 
  fillShape   t = drawRectangle (fill t)

extractVertexList :: (Real u, Floating u) => Rectangle u -> [Point2 u]
extractVertexList rect = [bl,br,tr,tl]
  where
    bl        = southwest rect
    tr        = northeast rect
    br        = southeast rect
    tl        = northwest rect
