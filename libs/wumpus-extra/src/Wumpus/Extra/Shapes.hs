{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Shapes
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Basic graphics objects (lines segments, curves, polygons...)
-- 
--------------------------------------------------------------------------------

module Wumpus.Extra.Shapes
  ( 

  -- * Data types
    Rectangle
  , rectangle
  , center
  , strokeRectangle

  ) where

import Wumpus.Core hiding ( center, CTM )
import Wumpus.Extra.Utils

import Data.AffineSpace         -- package: vector-space
import Data.VectorSpace


--------------------------------------------------------------------------------
-- Shapes will generally include a translation matrix...

type CTM u = Matrix3'3 u

rotateCTM :: (Floating u, Real u) => Radian -> CTM u -> CTM u
rotateCTM r m = rotationMatrix r * m

rotateAboutCTM :: (Floating u, Real u) => Radian -> Point2 u -> CTM u -> CTM u
rotateAboutCTM r pt m = originatedRotationMatrix r pt * m


--------------------------------------------------------------------------------

center :: forall u . (Num u, Fractional u) => Rectangle u -> Point2 u
center rtangle = (ctm *#) $  bl .+^ v  
  where
    bl        = rect_bottom_left rtangle
    tr        = rect_upper_right rtangle
    ctm       = rect_ctm         rtangle
    v         :: Vec2 u
    v         = (tr .-. bl) ^* 0.5

data Rectangle u = Rectangle 
       { rect_bottom_left     :: Point2 u
       , rect_upper_right     :: Point2 u
       , rect_ctm             :: CTM u
       }

type instance DUnit (Rectangle u) = u



instance (Floating u, Real u) => Rotate (Rectangle u) where
  rotate r = pstar (\m s -> s { rect_ctm = rotateCTM r m }) rect_ctm 

instance (Floating u, Real u) => RotateAbout (Rectangle u) where
  rotateAbout r pt = 
      pstar (\m s -> s { rect_ctm = rotateAboutCTM r pt m }) rect_ctm



-- | @rectangle : width * height * center_pt -> rectangle@
--
rectangle :: Fractional u => u -> u -> Point2 u -> Rectangle u
rectangle w h ctr = Rectangle (ctr .-^ v) (ctr .+^ v) identityMatrix
  where
    v = V2 (w * 0.5) (h * 0.5) 


-- | make Picture or Primitive?
--
strokeRectangle :: (Num u, Ord u) 
                => Rectangle u -> Primitive u
strokeRectangle rtangle = 
    cstroke () $ vertexPath $ pointwise (ctm *#) [bl,br,tr,tl]
  where
    bl        = rect_bottom_left rtangle
    tr        = rect_upper_right rtangle
    ctm       = rect_ctm         rtangle
    (V2 w h)  = tr .-. bl
    br        = bl .+^ (hvec w)
    tl        = bl .+^ (vvec h) 