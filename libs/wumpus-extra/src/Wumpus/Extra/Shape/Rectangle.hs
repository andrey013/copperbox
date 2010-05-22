{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Shape.Rectangle
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Coordinate points
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
import Data.VectorSpace


-- | Rectangles.
--
data Rectangle u = Rectangle 
      { rect_bottom_left      :: Point2 u
      , rect_upper_right      :: Point2 u
      , rect_ctm              :: CTM u
      , rect_label            :: Maybe (ShapeLabel)
      }

type instance DUnit (Rectangle u) = u


-- Having a vector to upper right seems a bit easier 
-- than representing it as a point...
--
withGeom :: Num u
         => (CTM u -> Point2 u -> Vec2 u -> a) -> Rectangle u -> a
withGeom f rect = f (rect_ctm rect) bl (tr .-. bl)
  where
    bl        = rect_bottom_left rect
    tr        = rect_upper_right rect

-- Instances 
  

instance (Fractional u) => AnchorCenter (Rectangle u) where
  center = withGeom $ \ ctm bl v -> 
      let v' = v ^* 0.5 in (ctm *#) $ (bl .+^ v')



instance (Fractional u) =>  AnchorCardinal (Rectangle u) where
  north = withGeom $ \ ctm bl (V2 w h) -> ctm *# (bl .+^ V2 (w*0.5) h)
  south = withGeom $ \ ctm bl (V2 w _) -> ctm *# (bl .+^ V2 (w*0.5) 0)
  east  = withGeom $ \ ctm bl (V2 w h) -> ctm *# (bl .+^ V2 w (h*0.5))
  west  = withGeom $ \ ctm bl (V2 _ h) -> ctm *# (bl .+^ V2 0 (h*0.5))

  northeast = withGeom $ \ ctm bl v -> ctm *#  (bl .+^ v)
  southeast = withGeom $ \ ctm bl (V2 w _) -> ctm *# (bl .+^ V2 w 0)
  southwest = withGeom $ \ ctm bl _ -> ctm *# bl
  northwest = withGeom $ \ ctm bl (V2 _ h) -> ctm *# (bl .+^ V2 0 h)


instance (Floating u, Real u) => Rotate (Rectangle u) where
  rotate r = pstar (\m s -> s { rect_ctm = rotateCTM r m }) rect_ctm 

instance (Floating u, Real u) => RotateAbout (Rectangle u) where
  rotateAbout r pt = 
      pstar (\m s -> s { rect_ctm = rotateAboutCTM r pt m }) rect_ctm

instance Num u => Scale (Rectangle u) where
  scale x y = pstar (\m s -> s { rect_ctm = scaleCTM x y m }) rect_ctm

instance Num u => Translate (Rectangle u) where
  translate x y = 
     pstar (\m s -> s { rect_ctm = translateCTM x y m }) rect_ctm


-- | @rectangle : width * height * center_pt -> rectangle@
--
rectangle :: Fractional u => u -> u -> Point2 u -> Rectangle u
rectangle w h ctr = Rectangle (ctr .-^ v) (ctr .+^ v) identityMatrix Nothing
  where
    v = V2 (w * 0.5) (h * 0.5) 



--  
--
strokeRectangle :: (Fractional u, Ord u, Stroke t) 
                => t -> Rectangle u -> Composite u
strokeRectangle t rect = fn rect $ \ctm bl (V2 w h) -> 
    labelledComposite ctm (bl .+^ V2 (w*0.5) (h*0.5)) (rect_label rect) shape
  where
    shape = cstroke t $ vertexPath $ extractVertexList rect

    fn a f = withGeom f a

fillRectangle :: (Fractional u, Ord u, Fill t) 
                => t -> Rectangle u -> Primitive u
fillRectangle t = fill t . vertexPath . extractVertexList


extractVertexList :: Fractional u => Rectangle u -> [Point2 u]
extractVertexList rect = [bl,br,tr,tl]
  where
    bl        = southwest rect
    tr        = northeast rect
    br        = southeast rect
    tl        = northwest rect
