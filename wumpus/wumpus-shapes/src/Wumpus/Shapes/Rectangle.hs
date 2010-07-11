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

-- | Rectangles.
--
data Rectangle u = Rectangle 
      { rect_center        :: Point2 u
      , rect_half_width    :: u
      , rect_half_height   :: u
      , rect_ctm           :: CTM u
      , rect_label         :: Maybe ShapeLabel
      }

type DRectangle = Rectangle Double

type instance DUnit (Rectangle u) = u




-- ctr * CTM * half_width * half_height      
--
withGeom :: Num u => (Point2 u -> CTM u -> u -> u -> a) -> Rectangle u -> a
withGeom f (Rectangle { rect_center=ctr, rect_ctm=ctm
                      , rect_half_width=hw, rect_half_height=hh}) =
    f ctr ctm hw hh
     
-- What to call this.... ?
calcPoint :: (Real u, Floating u) => (u -> u -> Vec2 u) -> Rectangle u -> Point2 u
calcPoint f = withGeom $ \(P2 cx cy) ctm hw hh -> 
    let (V2 x y) = f hw hh in translate cx cy $ ctmDisplace x y ctm

--------------------------------------------------------------------------------
-- Instances 
  

instance AnchorCenter (Rectangle u) where
  center (Rectangle { rect_center = ctr }) = ctr




instance (Real u, Floating u) =>  AnchorCardinal (Rectangle u) where
  north = calcPoint $ \ _  hh -> vvec hh
  south = calcPoint $ \ _  hh -> vvec (-hh)
  east  = calcPoint $ \ hw _  -> hvec hw
  west  = calcPoint $ \ hw _  -> hvec (-hw)

  northeast = calcPoint $ \ hw hh -> V2 hw hh
  southeast = calcPoint $ \ hw hh -> V2 hw (-hh)
  southwest = calcPoint $ \ hw hh -> V2 (-hw) (-hh)
  northwest = calcPoint $ \ hw hh -> V2 (-hw) hh



instance (Floating u, Real u) => Rotate (Rectangle u) where
  rotate r = star (\s ctm -> s {rect_ctm = rotateCTM r ctm })
                  rect_ctm


instance Num u => Scale (Rectangle u) where
  scale x y = star (\s ctm -> s { rect_ctm = scaleCTM x y ctm })
                   rect_ctm

instance Num u => Translate (Rectangle u) where
  translate x y = star (\s ctr -> s { rect_center = translate x y ctr} )
                       rect_center 


instance AddLabel (Rectangle u) where
  r `addLabel` text = star fn rect_label r
    where
      fn s Nothing    = s { rect_label = Just $ basicLabel text }
      fn s (Just lbl) = s { rect_label = Just $ updateText text lbl } 
     



--------------------------------------------------------------------------------
-- Construction

-- | @rectangle : width * height * center_pt -> rectangle@
--
rectangle :: Fractional u => u -> u -> Point2 u -> Rectangle u
rectangle w h ctr = Rectangle { rect_center       = ctr 
                              , rect_half_width   = 0.5*w 
                              , rect_half_height  = 0.5*h
                              , rect_ctm          = identityCTM 
                              , rect_label        = Nothing }


--------------------------------------------------------------------------------
-- Drawing 

--


drawRectangle :: (Real u, Floating u)   
              => (Path u -> Primitive u) -> Rectangle u -> Graphic u
drawRectangle drawF rect = labelpic . rectpic
  where
    labelpic = maybe id (labelGraphic (rect_center rect) (rect_ctm rect))
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
