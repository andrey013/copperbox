{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
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
  , rectangle_


  ) where

import Wumpus.Shapes.Base
import Wumpus.Shapes.Utils

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Basic.Anchors                     -- package: wumpus-basic
import Wumpus.Basic.Graphic hiding ( Rectangle, DRectangle )
import Wumpus.Basic.Graphic.DrawingAttr
import Wumpus.Basic.Monads.Drawing


-- | Rectangles.
--
data Rectangle u = Rectangle 
      { rect_half_width    :: u
      , rect_half_height   :: u
      , rect_ctm           :: CTM u
      , rect_label         :: Maybe ShapeLabel
      }

type DRectangle = Rectangle Double

type instance DUnit (Rectangle u) = u



updateCTM :: (CTM u -> CTM u) -> Rectangle u -> Rectangle u
updateCTM f = star (\s i -> s { rect_ctm = f i}) rect_ctm 



-- CTM * half_width * half_height      
--
withGeom :: Num u => (CTM u -> u -> u -> a) -> Rectangle u -> a
withGeom f (Rectangle { rect_ctm=ctm,rect_half_width=hw,rect_half_height=hh}) =
    f ctm hw hh
     
calcPoint :: (Real u, Floating u) => (u -> u -> Vec2 u) -> Rectangle u -> Point2 u
calcPoint f = withGeom $ \ctm hw hh -> 
    let (V2 x y) = f hw hh in ctmDisplace x y ctm

--------------------------------------------------------------------------------
-- Instances 
  

instance (Real u, Floating u) => CenterAnchor (Rectangle u) where
  center = ctmCenter . rect_ctm

instance (Real u, Floating u) => CardinalAnchor (Rectangle u) where
  north = calcPoint $ \ _  hh -> vvec hh
  south = calcPoint $ \ _  hh -> vvec (-hh)
  east  = calcPoint $ \ hw _  -> hvec hw
  west  = calcPoint $ \ hw _  -> hvec (-hw)

instance (Real u, Floating u) => CardinalAnchor2 (Rectangle u) where
  northeast = calcPoint $ \ hw hh -> V2 hw hh
  southeast = calcPoint $ \ hw hh -> V2 hw (-hh)
  southwest = calcPoint $ \ hw hh -> V2 (-hw) (-hh)
  northwest = calcPoint $ \ hw hh -> V2 (-hw) hh


instance (Floating u, Real u) => Rotate (Rectangle u) where
  rotate r = updateCTM (rotateCTM r)
 
instance Num u => Scale (Rectangle u) where
  scale x y = updateCTM (scaleCTM x y)

instance Num u => Translate (Rectangle u) where
  translate x y = updateCTM (translateCTM x y)

     



--------------------------------------------------------------------------------
-- Construction

-- | @rectangle : width * height -> rectangle@
--
rectangle :: Fractional u => u -> u -> Rectangle u
rectangle w h = Rectangle { rect_half_width   = 0.5*w 
                          , rect_half_height  = 0.5*h
                          , rect_ctm          = identityCTM 
                          , rect_label        = Nothing }


rectangle_ :: Fractional u => u -> u -> String -> Rectangle u
rectangle_ w h str = (rectangle w h) { rect_label = Just $ ShapeLabel str } 


-- /fill/ & /stroke/ are probably lifters to AGraphic...


strokeR :: (Real u, Floating u)
        => DrawingAttr -> Point2 u -> Rectangle u -> Graphic u
strokeR attr (P2 x y) = 
    wrapG . cstroke (strokeAttr attr) . rectPath . translate x y 
                       

fillR :: (Real u, Floating u) 
      => DrawingAttr -> Point2 u -> Rectangle u -> Graphic u
fillR attr (P2 x y) = 
    wrapG . fill (fillAttr attr) . rectPath . translate x y

textR :: (Real u, Floating u, FromPtSize u) 
      => DrawingAttr -> Point2 u -> Rectangle u -> Graphic u
textR attr (P2 x y) rect = maybe id sk $ rect_label rect
  where
    ctm      = rect_ctm $ translate x y rect
    sk label = labelGraphic label (textAttr attr) ctm 
    
make :: (Real u, Floating u) 
     => DrawingAttr -> Point2 u -> Rectangle u -> Rectangle u
make _ (P2 x y) = translate x y

instance (Real u, Floating u, FromPtSize u) => Draw (Rectangle u) where
  draw rect = AGraphic id df (\a p -> make a p rect)
    where
      df attr pt = textR attr pt rect . strokeR attr pt rect 
                                      . fillR attr pt rect
      


--------------------------------------------------------------------------------
-- Paths...



rectPath :: (Real u, Floating u) => Rectangle u -> Path u
rectPath = vertexPath . extractVertexPoints

extractVertexPoints :: (Real u, Floating u) => Rectangle u -> [Point2 u]
extractVertexPoints rect = [bl,br,tr,tl]
  where
    bl        = southwest rect
    tr        = northeast rect
    br        = southeast rect
    tl        = northwest rect
